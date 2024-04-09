use std::io::BufRead;

use crate::{
    CachedEvent, CachedToken, Delimiter, Error, Event, Scanner, SequenceStyle, SourceLocation,
    Span, Spanned, SpannedExt, Token, TokenType,
};

/// Streaming parser that turns [`Token`]s into [`Event`]s.
///
/// This validates the stream of tokens, but does not perform any high-level
/// transformations, such as organizing key-value pairs into maps or
/// interpreting escape sequences within strings.
pub struct ParseStream<R> {
    scanner: Scanner<R>,
    inner: ParserInner,
    pending_consume: Consume,
    eof: bool,
}

#[derive(Default)]
struct ParserInner {
    lookahead: Lookahead,
    root: CurlySequenceState,
    stack: Vec<ParserState>,
    pending_anchor: PendingAnchor,
}

#[derive(Debug, thiserror::Error, PartialEq)]
pub enum ParserError {
    #[error("Missing value in key-value pair")]
    MissingValue,
    #[error("Unclosed delimiter: {0} at {1}")]
    UnclosedDelimiter(TokenType, SourceLocation),
    #[error("Unexpected token: {0} at {1}")]
    UnexpectedToken(TokenType, SourceLocation),
    #[error("Lists in square brackets or parentheses must be separated by commas: {0}")]
    MissingCommaInList(SourceLocation),
    #[error("Anchors must be followed by a value: {0}")]
    TrailingAnchor(SourceLocation),
    #[error("values can only have one anchor: {0}")]
    MultipleAnchors(SourceLocation),
    #[error("reference cannot have anchors: {0}")]
    ReferenceWithAnchor(SourceLocation),
}

enum ParserState {
    CurlySequence(CurlySequenceState),
    SquareSequence(SquareSequenceState),
    ParensSequence(ParensSequenceState),
    TaggedValue(TaggedValueState),
}

enum Consume {
    Consume2,
    Consume1,
    Consume0,
    Eof,
}

impl<R: BufRead> ParseStream<R> {
    pub fn new(reader: R) -> Self {
        Self {
            scanner: Scanner::new(reader),
            inner: ParserInner::default(),
            // Initially populate the lookahead buffer with two tokens.
            pending_consume: Consume::Consume2,
            eof: false,
        }
    }

    pub fn next_event(&mut self, out: &mut CachedEvent) -> Result<(), Error> {
        let Self {
            scanner,
            inner,
            pending_consume,
            eof,
        } = self;

        out.clear();

        if *eof {
            return Ok(());
        }

        loop {
            match pending_consume {
                Consume::Consume0 => {}
                Consume::Consume1 => {
                    scanner.next_token(inner.lookahead.write_next())?;
                }
                Consume::Consume2 => {
                    scanner.next_token(inner.lookahead.write_next())?;
                    scanner.next_token(inner.lookahead.write_next())?;
                }
                Consume::Eof => unreachable!("unexpected EOF"),
            }

            match inner.parse_next(out)? {
                Consume::Eof => {
                    *eof = !out.is_some();
                    return Ok(());
                }
                consume => {
                    *pending_consume = consume;
                    if out.is_some() {
                        return Ok(());
                    } else {
                        // Did not produce an event, just consume tokens and continue.
                        continue;
                    }
                }
            }
        }
    }

    pub fn expect_event(&mut self, out: &mut CachedEvent) -> Result<(), Error> {
        self.next_event(out)?;
        if !out.is_some() {
            return Err(Error::Scanner(crate::ScannerError::UnexpectedEof));
        }
        Ok(())
    }
}

impl ParserInner {
    fn parse_next(&mut self, out: &mut CachedEvent) -> Result<Consume, ParserError> {
        if let Some(state) = self.stack.last() {
            match state {
                ParserState::CurlySequence(_) => self.parse_curly_sequence_item(out),
                ParserState::SquareSequence(_) => self.parse_square_sequence_item(out),
                ParserState::ParensSequence(_) => self.parse_parens_sequence_item(out),
                ParserState::TaggedValue(_) => self.parse_value_after_tag_anchor(out),
            }
        } else {
            self.parse_curly_sequence_item(out)
        }
    }

    fn parse_curly_sequence_item(&mut self, out: &mut CachedEvent) -> Result<Consume, ParserError> {
        let (state, is_root) = match self.stack.last_mut() {
            Some(ParserState::CurlySequence(state)) => (state, false),
            None => (&mut self.root, true),
            _ => panic!("inconsistent parser state"),
        };

        let [Some((token, span)), next] = self.lookahead.peek2() else {
            if is_root {
                return Ok(Consume::Eof);
            } else {
                return Err(ParserError::UnclosedDelimiter(
                    TokenType::SequenceStartCurly,
                    state.start.start,
                ));
            }
        };

        if state.need_delimiter {
            match token {
                Token::Delimiter(..) => {
                    state.need_delimiter = false;
                    return Ok(Consume::Consume1);
                }
                Token::Comment(comment) => {
                    state.need_delimiter = false;
                    out.set(Some(Event::Comment(comment.in_span(span))));
                    return Ok(Consume::Consume1);
                }
                Token::SequenceEndCurly => {}
                _ => return Err(ParserError::UnexpectedToken(token.ty(), span.start)),
            }
        }

        if !state.parsing_kv_value {
            // Parse a key or a keyless value.
            Ok(match (token, next.map(|(t, _)| t)) {
                (Token::Comment(comment), _) => {
                    out.set(Some(Event::Comment(comment.in_span(span))));
                    Consume::Consume1
                }
                // Skip newlines.
                (Token::Delimiter(Delimiter::Newline), _) => Consume::Consume1,
                // Explicit empty key.
                (Token::KeySigil, Some(Token::Colon)) if !state.next_is_key => {
                    state.parsing_kv_value = true;
                    state.did_parse_colon = true;
                    state.next_is_key = false;
                    let colon_span = next.map(|(_, span)| span).unwrap();
                    out.set(Some(Event::Empty(span.merge(&colon_span))));
                    Consume::Consume2
                }
                (Token::KeySigil, Some(_)) if !state.next_is_key => {
                    state.next_is_key = true;
                    state.did_parse_colon = false;
                    state.has_explicit_key_sigil = true;
                    Consume::Consume1
                }
                (Token::Scalar(scalar), Some(Token::Colon)) if !state.next_is_key => {
                    state.parsing_kv_value = true;
                    state.did_parse_colon = true;
                    state.next_is_key = false;
                    out.set(Some(Event::Scalar(scalar.in_span(span))));
                    Consume::Consume2
                }
                (Token::SequenceEndCurly, _) => {
                    self.stack.pop();
                    out.set(Some(Event::EndMapping(span)));
                    Consume::Consume1
                }
                (Token::Merge(anchor), _) => {
                    out.set(Some(Event::Merge(anchor.in_span(span))));
                    Consume::Consume1
                }
                _ => {
                    state.parsing_kv_value = true;
                    if state.next_is_key {
                        state.did_parse_colon = false;
                        state.next_is_key = false;
                        return Self::do_parse_value(
                            ((token, span), next),
                            out,
                            &mut self.pending_anchor,
                            &mut self.stack,
                        );
                    } else {
                        // Emit artificial empty key, and pretend we parsed a
                        // colon, since the next value is keyless.
                        state.did_parse_colon = true;
                        out.set(Some(Event::Empty(Span::empty(span.start))));
                        Consume::Consume0
                    }
                }
            })
        } else {
            // Parse the right-hand side of a key-value pair.
            Ok(match (token, next.map(|(t, _)| t)) {
                (Token::Delimiter(Delimiter::Newline), _) if state.has_explicit_key_sigil => {
                    Consume::Consume1
                }
                (Token::Comment(comment), _) => {
                    out.set(Some(Event::Comment(comment.in_span(span))));
                    Consume::Consume1
                }
                (Token::Colon, Some(Token::Delimiter(Delimiter::Newline)))
                    if !state.did_parse_colon =>
                {
                    state.has_explicit_key_sigil = false;
                    state.parsing_kv_value = false;
                    out.set(Some(Event::Empty(Span::empty(span.start))));
                    Consume::Consume2
                }
                (Token::Colon, Some(_)) if !state.did_parse_colon => {
                    state.did_parse_colon = true;
                    state.has_explicit_key_sigil = false;
                    Consume::Consume1
                }
                _ if state.did_parse_colon => {
                    state.has_explicit_key_sigil = false;
                    state.parsing_kv_value = false;
                    state.did_parse_colon = false;
                    state.need_delimiter = true;
                    return Self::do_parse_value(
                        ((token, span), next),
                        out,
                        &mut self.pending_anchor,
                        &mut self.stack,
                    );
                }
                _ => {
                    return Err(ParserError::UnexpectedToken(token.ty(), span.start));
                }
            })
        }
    }

    fn parse_square_sequence_item(
        &mut self,
        out: &mut CachedEvent,
    ) -> Result<Consume, ParserError> {
        let state = match self.stack.last_mut() {
            Some(ParserState::SquareSequence(state)) => state,
            _ => panic!("inconsistent parser state"),
        };

        let [Some((token, span)), next] = self.lookahead.peek2() else {
            return Err(ParserError::UnclosedDelimiter(
                TokenType::SequenceStartSquare,
                state.start.start,
            ));
        };

        Ok(match (token, next.map(|(t, _)| t)) {
            (Token::Comment(comment), _) => {
                out.set(Some(Event::Comment(comment.in_span(span))));
                Consume::Consume1
            }
            (Token::Delimiter(Delimiter::Comma), _) => Consume::Consume1,
            (
                Token::Delimiter(Delimiter::Newline),
                Some(Token::SequenceEndSquare | Token::Comment(..)),
            ) => Consume::Consume1,
            (Token::Delimiter(Delimiter::Newline), _) => {
                return Err(ParserError::MissingCommaInList(span.start))
            }
            (Token::SequenceEndSquare, _) => {
                self.stack.pop();
                out.set(Some(Event::EndSequence(span)));
                Consume::Consume1
            }
            (Token::Merge(anchor), _) => {
                out.set(Some(Event::Merge(anchor.in_span(span))));
                Consume::Consume1
            }
            _ => {
                return Self::do_parse_value(
                    ((token, span), next),
                    out,
                    &mut self.pending_anchor,
                    &mut self.stack,
                )
            }
        })
    }

    fn parse_parens_sequence_item(
        &mut self,
        out: &mut CachedEvent,
    ) -> Result<Consume, ParserError> {
        let state = match self.stack.last_mut() {
            Some(ParserState::ParensSequence(state)) => state,
            _ => panic!("inconsistent parser state"),
        };

        let [Some((token, span)), next] = self.lookahead.peek2() else {
            return Err(ParserError::UnclosedDelimiter(
                TokenType::SequenceStartSquare,
                state.start.start,
            ));
        };

        Ok(match (token, next.map(|(t, _)| t)) {
            (Token::Comment(comment), _) => {
                out.set(Some(Event::Comment(comment.in_span(span))));
                Consume::Consume1
            }
            (Token::Delimiter(Delimiter::Comma), _) => Consume::Consume1,
            (
                Token::Delimiter(Delimiter::Newline),
                Some(Token::SequenceEndParens | Token::Comment(..)),
            ) => Consume::Consume1,
            (Token::Delimiter(Delimiter::Newline), _) => {
                return Err(ParserError::MissingCommaInList(span.start))
            }
            (Token::SequenceEndParens, _) => {
                self.stack.pop();
                out.set(Some(Event::EndSequence(span)));
                Consume::Consume1
            }
            (Token::Merge(anchor), _) => {
                out.set(Some(Event::Merge(anchor.in_span(span))));
                Consume::Consume1
            }
            _ => {
                return Self::do_parse_value(
                    ((token, span), next),
                    out,
                    &mut self.pending_anchor,
                    &mut self.stack,
                )
            }
        })
    }

    fn do_parse_value<'r>(
        ((token, span), next): ((Token<'r>, Span), Option<(Token<'r>, Span)>),
        out: &mut CachedEvent,
        pending_anchor: &'r mut PendingAnchor,
        stack: &mut Vec<ParserState>,
    ) -> Result<Consume, ParserError> {
        match (token, next.map(|(t, _)| t)) {
            (Token::Anchor(_), None) => return Err(ParserError::TrailingAnchor(span.start)),
            (
                Token::Anchor(_),
                Some(Token::SequenceEndCurly | Token::SequenceEndSquare | Token::SequenceEndParens),
            ) => {
                return Err(ParserError::TrailingAnchor(span.start));
            }
            (Token::Anchor(_), Some(Token::Ref(_) | Token::Merge(_))) => {
                return Err(ParserError::ReferenceWithAnchor(span.start));
            }
            (Token::Anchor(tag), Some(_)) => {
                if pending_anchor.is_some() {
                    return Err(ParserError::MultipleAnchors(span.start));
                }
                stack.push(ParserState::TaggedValue(TaggedValueState {}));
                pending_anchor.set(tag, span);
                return Ok(Consume::Consume1);
            }

            _ => (),
        }

        Self::do_parse_untagged_value(((token, span), next), out, pending_anchor, stack)
    }

    fn parse_value_after_tag_anchor(
        &mut self,
        out: &mut CachedEvent,
    ) -> Result<Consume, ParserError> {
        match self.stack.pop() {
            Some(ParserState::TaggedValue(_)) => (),
            _ => panic!("inconsistent parser state"),
        };

        self.parse_untagged_value(out)
    }

    fn parse_untagged_value(&mut self, out: &mut CachedEvent) -> Result<Consume, ParserError> {
        let [Some((token, span)), next] = self.lookahead.peek2() else {
            unreachable!("eof not handled in parent parser");
        };

        Self::do_parse_untagged_value(
            ((token, span), next),
            out,
            &mut self.pending_anchor,
            &mut self.stack,
        )
    }

    // Implementation of `parse_untagged_value`. This is just here to work
    // around the borrow checker. Will be fixed with polonius.
    fn do_parse_untagged_value(
        ((token, span), next): ((Token, Span), Option<(Token, Span)>),
        out: &mut CachedEvent,
        pending_anchor: &mut PendingAnchor,
        stack: &mut Vec<ParserState>,
    ) -> Result<Consume, ParserError> {
        Ok(match (token, next) {
            (Token::Comment(comment), _) => {
                out.set(Some(Event::Comment(comment.in_span(span))));
                Consume::Consume1
            }
            // Bare sequences.
            (Token::SequenceStartCurly, _) => {
                stack.push(ParserState::CurlySequence(CurlySequenceState {
                    start: span,
                    ..Default::default()
                }));
                out.set(Some(Event::BeginMapping {
                    span,
                    type_tag: None,
                    anchor: pending_anchor.take(),
                }));
                Consume::Consume1
            }
            (Token::SequenceStartSquare, _) => {
                stack.push(ParserState::SquareSequence(SquareSequenceState {
                    start: span,
                    ..Default::default()
                }));
                out.set(Some(Event::BeginSequence {
                    span,
                    style: SequenceStyle::List,
                    type_tag: None,
                    anchor: pending_anchor.take(),
                }));
                Consume::Consume1
            }
            (Token::SequenceStartParens, _) => {
                stack.push(ParserState::ParensSequence(ParensSequenceState {
                    start: span,
                    ..Default::default()
                }));
                out.set(Some(Event::BeginSequence {
                    span,
                    style: SequenceStyle::Tuple,
                    type_tag: None,
                    anchor: pending_anchor.take(),
                }));
                Consume::Consume1
            }
            // Type-tagged sequences.
            (Token::Scalar(scalar), Some((Token::SequenceStartCurly, start_span))) => {
                stack.push(ParserState::CurlySequence(CurlySequenceState {
                    start: start_span,
                    ..Default::default()
                }));
                out.set(Some(Event::BeginMapping {
                    span: start_span,
                    type_tag: Some(scalar.in_span(span)),
                    anchor: pending_anchor.take(),
                }));
                Consume::Consume2
            }
            (Token::Scalar(scalar), Some((Token::SequenceStartSquare, start_span))) => {
                stack.push(ParserState::SquareSequence(SquareSequenceState {
                    start: start_span,
                    ..Default::default()
                }));
                out.set(Some(Event::BeginSequence {
                    span: start_span,
                    style: SequenceStyle::List,
                    type_tag: Some(scalar.in_span(span)),
                    anchor: pending_anchor.take(),
                }));
                Consume::Consume2
            }
            (Token::Scalar(scalar), Some((Token::SequenceStartParens, start_span))) => {
                stack.push(ParserState::ParensSequence(ParensSequenceState {
                    start: start_span,
                    ..Default::default()
                }));
                out.set(Some(Event::BeginSequence {
                    span: start_span,
                    style: SequenceStyle::Tuple,
                    type_tag: Some(scalar.in_span(span)),
                    anchor: pending_anchor.take(),
                }));
                Consume::Consume2
            }
            // Bare scalars.
            (Token::Scalar(scalar), _) => {
                out.set(Some(Event::Scalar(
                    scalar
                        .with_anchor(pending_anchor.take().map(Spanned::into_inner))
                        .in_span(span),
                )));
                Consume::Consume1
            }
            // References
            (Token::Ref(anchor), _) => {
                out.set(Some(Event::Ref(anchor.in_span(span))));
                Consume::Consume1
            }
            (unexpected, _) => {
                return Err(ParserError::UnexpectedToken(unexpected.ty(), span.start))
            }
        })
    }
}

#[derive(Default)]
struct CurlySequenceState {
    start: Span,
    parsing_kv_value: bool,
    next_is_key: bool,
    has_explicit_key_sigil: bool,
    did_parse_colon: bool,
    need_delimiter: bool,
}

#[derive(Default)]
struct SquareSequenceState {
    start: Span,
}

#[derive(Default)]
struct ParensSequenceState {
    start: Span,
}

#[derive(Default)]
struct TaggedValueState {}

#[derive(Default)]
struct Lookahead {
    cached: [CachedToken; 2],
}

impl Lookahead {
    pub fn peek2(&self) -> [Option<(Token, Span)>; 2] {
        [self.cached[0].get(), self.cached[1].get()]
    }

    pub fn write_next(&mut self) -> &mut CachedToken {
        self.cached.swap(0, 1);
        &mut self.cached[1]
    }
}

trait Consuming {
    fn consume1(self) -> (Option<Self>, Consume)
    where
        Self: Sized,
    {
        (Some(self), Consume::Consume1)
    }
    fn consume2(self) -> (Option<Self>, Consume)
    where
        Self: Sized,
    {
        (Some(self), Consume::Consume2)
    }
}

impl<T> Consuming for T {}

#[derive(Default)]
struct PendingAnchor {
    buf: String,
    span: Option<Span>,
}

impl PendingAnchor {
    #[inline]
    fn take(&mut self) -> Option<Spanned<&str>> {
        self.span.take().map(|span| self.buf.as_str().in_span(span))
    }

    #[inline]
    fn set(&mut self, value: &str, span: Span) {
        assert!(self.span.is_none());
        self.buf.clear();
        self.buf.push_str(value);
        self.span = Some(span);
    }

    #[inline]
    fn is_some(&self) -> bool {
        self.span.is_some()
    }
}

#[cfg(test)]
mod tests {
    use crate::{Scalar, ScalarStyle};

    use super::*;

    #[track_caller]
    fn assert_events_eq(input: &str, expected: &[Event]) {
        let mut parser = ParseStream::new(input.as_bytes());
        let mut events = vec![];
        let mut event = CachedEvent::default();
        loop {
            match parser.next_event(&mut event) {
                Ok(()) => {
                    if event.is_some() {
                        events.push(std::mem::take(&mut event))
                    } else {
                        break;
                    }
                }
                Err(e) => panic!("error: {}", e),
            }
        }
        assert_eq!(events, expected);
    }

    #[track_caller]
    fn assert_events_err(input: &str, expected: ParserError) {
        let mut parser = ParseStream::new(input.as_bytes());
        let mut events = vec![];
        let mut event = CachedEvent::default();
        loop {
            match parser.next_event(&mut event) {
                Ok(_) => {
                    if event.is_some() {
                        events.push(std::mem::take(&mut event))
                    } else {
                        break;
                    }
                }
                Err(e) => {
                    assert_eq!(e, expected);
                    return;
                }
            }
        }

        panic!("did not get an error, expected {expected:?}")
    }

    #[test]
    fn single_scalar() {
        assert_events_eq("foo", &[Event::empty(), Event::plain("foo")]);
    }

    #[test]
    fn two_scalars() {
        assert_events_err(
            "foo bar",
            ParserError::UnexpectedToken(
                TokenType::Scalar(ScalarStyle::Plain),
                SourceLocation {
                    offset: 4,
                    line: 0,
                    column: 4,
                },
            ),
        );
    }

    #[test]
    fn type_tagged_sequence() {
        assert_events_eq(
            "foo {}",
            &[
                Event::empty(),
                Event::begin_mapping(Some("foo"), None),
                Event::end_mapping(),
            ],
        );

        assert_events_eq(
            "foo []",
            &[
                Event::empty(),
                Event::begin_sequence(SequenceStyle::List, Some("foo"), None),
                Event::end_sequence(),
            ],
        );

        assert_events_eq(
            "foo()",
            &[
                Event::empty(),
                Event::begin_sequence(SequenceStyle::Tuple, Some("foo"), None),
                Event::end_sequence(),
            ],
        );
    }

    #[test]
    fn two_kv_pairs() {
        assert_events_eq(
            "foo: bar\nbaz: qux\n\r\n\n\n",
            &[
                Event::plain("foo"),
                Event::plain("bar"),
                Event::plain("baz"),
                Event::plain("qux"),
            ],
        );
    }

    #[test]
    fn complex() {
        const INPUT: &str = r#"
    "normal item"
    "key": value

    {
        1: 2

        Object {
            "key": value
        }

        List [
            1.0,
            2.0,
            3.0,
            &ref,
        ]

        @anchor Enum(hello, world)

        *merge
    }
            "#;

        assert_events_eq(
            INPUT,
            &[
                Event::empty(), // implicit empty key
                Event::quoted("normal item"),
                Event::quoted("key"),
                Event::plain("value"),
                Event::empty(), // implicit empty key
                Event::begin_mapping(None, None),
                Event::plain("1"),
                Event::plain("2"),
                Event::empty(), // implicit empty key
                Event::begin_mapping(Some("Object"), None),
                Event::quoted("key"),
                Event::plain("value"),
                Event::end_mapping(),
                Event::empty(), // implicit empty key
                Event::begin_sequence(SequenceStyle::List, Some("List"), None),
                Event::plain("1.0"),
                Event::plain("2.0"),
                Event::plain("3.0"),
                Event::reference("ref"),
                Event::end_sequence(),
                Event::empty(), // implicit empty key
                Event::begin_sequence(SequenceStyle::Tuple, Some("Enum"), Some("anchor")),
                Event::plain("hello"),
                Event::plain("world"),
                Event::end_sequence(),
                Event::merge("merge"),
                Event::end_mapping(),
            ],
        );
    }

    #[test]
    fn complex_anchors() {
        let input = r#"
            ? @baz baz: @forty-two 42
        "#;

        assert_events_eq(
            input,
            &[
                Event::scalar(Scalar::plain("baz").with_anchor("baz")),
                Event::scalar(Scalar::plain("42").with_anchor("forty-two")),
            ],
        )
    }

    #[test]
    fn comments() {
        let input = r#"
            # comment
            foo: bar # comment
            # comment
        "#;

        assert_events_eq(
            input,
            &[
                Event::comment(" comment"),
                Event::plain("foo"),
                Event::plain("bar"),
                Event::comment(" comment"),
                Event::comment(" comment"),
            ],
        );

        let input = r#"
        foo: [1, 2,
        #comment
        3]
        "#;

        assert_events_eq(
            input,
            &[
                Event::plain("foo"),
                Event::begin_sequence(SequenceStyle::List, None, None),
                Event::plain("1"),
                Event::plain("2"),
                Event::comment("comment"),
                Event::plain("3"),
                Event::end_sequence(),
            ],
        );
    }
}
