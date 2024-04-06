use polonius_the_crab::*;
use std::io::BufRead;

use crate::{
    error::Error, CharExt, Delimiter, Input, InputBuffer, NeedMore, Scalar, ScalarStyle,
    SourceLocation, Spanned, SpannedExt, Token,
};

pub struct Scanner<R> {
    reader: R,
    buffer: InputBuffer,
    inner: ScannerInner,
    eof: bool,
}

#[derive(Default)]
struct ScannerInner {
    stack: Vec<State>,
    current_token: String,
    root_has_items: bool,
}

#[derive(Debug, thiserror::Error, PartialEq)]
pub enum ScannerError {
    #[error("unexpected char: {0:?}")]
    UnexpectedChar(char),
    #[error("unexpected EOF")]
    UnexpectedEof,
}

#[derive(Debug)]
enum Status {
    NeedMore(NeedMore),
    Error(Error),
}

impl From<NeedMore> for Status {
    #[inline]
    fn from(need_more: NeedMore) -> Self {
        Status::NeedMore(need_more)
    }
}

impl<E> From<E> for Status
where
    Error: From<E>,
{
    #[inline]
    fn from(e: E) -> Self {
        Status::Error(Error::from(e))
    }
}

impl From<std::io::ErrorKind> for Error {
    #[inline]
    fn from(kind: std::io::ErrorKind) -> Self {
        Error::Io(std::io::Error::from(kind))
    }
}

#[derive(Clone, Copy)]
struct State {
    start: SourceLocation,
    state: StateType,
}

#[derive(Clone, Copy)]
enum StateType {
    Delimiter(DelimiterState),
    Sequence(SequenceState),
    PlainScalar,
    QuotedScalar(TrimMode),
    SingleQuotedScalar,
    Tag,
    Ref,
    Merge,
    Comment,
}

#[derive(Clone, Copy, Default)]
struct SequenceState {
    has_items: bool,
}

#[derive(Clone, Copy, Default)]
struct DelimiterState {
    newlines: usize,
    comma: bool,
}

#[derive(Clone, Copy)]
enum TrimMode {
    None,
    Whitespace,
    Indentation,
}

impl<R: BufRead> Scanner<R> {
    pub fn new(reader: R) -> Self {
        Self {
            reader,
            buffer: InputBuffer::default(),
            inner: ScannerInner::default(),
            eof: false,
        }
    }

    pub fn next_token(&mut self) -> Result<Option<Spanned<Token>>, Error> {
        let Self {
            reader,
            buffer,
            inner,
            eof,
        } = self;
        let mut inner = &mut *inner;

        // Need polonius here to extend the lifetime of the `Token`, which
        // contains a reference into `inner`, but current borrowck gets confused due to the loop.
        polonius_loop!(
            |inner| -> Result<Option<Spanned<Token<'polonius>>>, Error> {
                match inner.next_token(buffer) {
                    Ok(Some(token)) => polonius_return!(Ok(Some(token))),
                    Ok(None) => polonius_return!(Ok(None)),
                    Err(Status::NeedMore(NeedMore(at_least))) => {
                        match Self::read_chars(reader, buffer, at_least, eof) {
                            Ok(()) => polonius_continue!(),
                            Err(e) => polonius_return!(Err(e)),
                        }
                    }
                    Err(Status::Error(e)) => polonius_return!(Err(e)),
                }
            }
        )
    }

    fn read_chars(
        reader: &mut R,
        buffer: &mut InputBuffer,
        at_least: usize,
        eof: &mut bool,
    ) -> Result<(), Error> {
        if *eof {
            if at_least > 0 {
                return Err(std::io::ErrorKind::UnexpectedEof.into());
            } else {
                return Ok(());
            }
        }

        while buffer.len() < at_least {
            let bytes = reader.fill_buf()?;

            if bytes.is_empty() {
                buffer.push_eof();
                *eof = true;
                return Ok(());
            }

            match std::str::from_utf8(bytes) {
                Ok(s) => {
                    let len = bytes.len();
                    buffer.push_str(s);
                    reader.consume(len);
                }
                Err(e) => {
                    let len = e.valid_up_to();
                    if e.error_len().is_some() {
                        return Err(e.into());
                    }
                    buffer.push_str(std::str::from_utf8(&bytes[..len]).unwrap());
                    reader.consume(len);
                }
            }
        }

        Ok(())
    }
}

impl ScannerInner {
    fn next_token<'r>(
        &'r mut self,
        input: &mut InputBuffer,
    ) -> Result<Option<Spanned<Token<'r>>>, Status> {
        let Some(state) = self.stack.last() else {
            return self.scan_sequence(input);
        };

        match state.state {
            StateType::Delimiter(_) => self.scan_delimiter(input),
            StateType::Sequence(_) => self.scan_sequence(input),
            StateType::PlainScalar => self.scan_plain(input).map(Some),
            StateType::QuotedScalar(_) => self.scan_quoted(input).map(Some),
            StateType::SingleQuotedScalar => self.scan_quoted(input).map(Some),
            StateType::Tag => self.scan_tag(input).map(Some),
            StateType::Ref => self.scan_ref(input).map(Some),
            StateType::Merge => self.scan_merge(input).map(Some),
            StateType::Comment => self.scan_comment(input).map(Some),
        }
    }

    fn scan_delimiter<'r>(
        &'r mut self,
        input: &mut InputBuffer,
    ) -> Result<Option<Spanned<Token<'r>>>, Status> {
        let Some(State {
            state: StateType::Delimiter(state),
            ..
        }) = self.stack.last_mut()
        else {
            panic!("inconsistent scanner state");
        };

        loop {
            match input.peek()? {
                Input::Value(' ' | '\t') => {
                    _ = input.pop();
                    continue;
                }
                Input::Value(',') => {
                    _ = input.pop();

                    if state.comma {
                        // More than one comma is not allowed.
                        return Err(ScannerError::UnexpectedChar(',').into());
                    }
                    state.comma = true;
                    continue;
                }
                Input::Value('\n') => {
                    _ = input.pop();
                    state.newlines += 1;
                    continue;
                }
                Input::Value('\r') => {
                    _ = input.pop();
                    continue;
                }
                Input::Value('#') => {
                    // Comments always end with a newline.
                    state.newlines += 1;

                    _ = input.pop();
                    self.stack.push(State {
                        start: input.current_location(),
                        state: StateType::Comment,
                    });
                    self.current_token.clear();
                    return self.scan_comment(input).map(Some);
                }
                _ => break,
            }
        }

        let Some(State {
            state: StateType::Delimiter(state),
            start,
        }) = self.stack.pop()
        else {
            panic!("inconsistent scanner state");
        };
        let end = input.current_location();

        if state.comma {
            return Ok(Some(Token::Delimiter(Delimiter::Comma).in_span(start..end)));
        }
        if state.newlines > 0 {
            return Ok(Some(
                Token::Delimiter(Delimiter::Newline).in_span(start..end),
            ));
        }
        if input.is_eof() {
            return Ok(None);
        }

        panic!("inconsistent scanner state; scan_delimiter()")
    }

    fn set_current_sequence_has_items(&mut self) {
        match self.stack.last_mut() {
            Some(State {
                state: StateType::Sequence(SequenceState { has_items }),
                ..
            }) => *has_items = true,
            None => self.root_has_items = true,
            _ => panic!("inconsistent scanner state"),
        }
    }

    fn scan_sequence<'r>(
        &'r mut self,
        input: &mut InputBuffer,
    ) -> Result<Option<Spanned<Token<'r>>>, Status> {
        let is_root = self.stack.is_empty();

        let is_initial = match self.stack.last() {
            Some(State {
                state: StateType::Sequence(state),
                ..
            }) => !state.has_items,
            None => !self.root_has_items,
            _ => panic!("inconsistent scanner state"),
        };

        loop {
            match input.peek()? {
                Input::Eof if is_root => {
                    return Ok(None);
                }
                Input::Eof => return Err(Status::Error(std::io::ErrorKind::UnexpectedEof.into())),
                Input::Value(ch) => match ch {
                    '}' if is_root => {
                        return Err(ScannerError::UnexpectedChar('}').into());
                    }
                    ']' if is_root => {
                        return Err(ScannerError::UnexpectedChar(']').into());
                    }
                    ')' if is_root => {
                        return Err(ScannerError::UnexpectedChar(')').into());
                    }
                    '}' => {
                        self.stack.pop().expect("empty stack");

                        let start = input.current_location();
                        _ = input.pop();
                        let end = input.current_location();

                        return Ok(Some(Token::SequenceEndCurly.in_span(start..end)));
                    }
                    ']' => {
                        self.stack.pop().expect("empty stack");

                        let start = input.current_location();
                        _ = input.pop();
                        let end = input.current_location();

                        return Ok(Some(Token::SequenceEndSquare.in_span(start..end)));
                    }
                    ')' => {
                        self.stack.pop().expect("empty stack");
                        let start = input.current_location();
                        _ = input.pop();
                        let end = input.current_location();

                        return Ok(Some(Token::SequenceEndParens.in_span(start..end)));
                    }
                    '#' => {
                        _ = input.pop();
                        self.stack.push(State {
                            start: input.current_location(),
                            state: StateType::Comment,
                        });
                        self.current_token.clear();
                        return self.scan_comment(input).map(Some);
                    }
                    '?' => {
                        let start = input.current_location();
                        _ = input.pop();
                        let end = input.current_location();
                        return Ok(Some(Token::KeySigil.in_span(start..end)));
                    }
                    ':' if !is_initial => {
                        let start = input.current_location();
                        _ = input.pop();
                        let end = input.current_location();
                        return Ok(Some(Token::Colon.in_span(start..end)));
                    }
                    '\n' | '\r' if is_initial => {
                        _ = input.pop();
                        continue;
                    }
                    ',' | '\n' | '\r' if !is_initial => {
                        self.stack.push(State {
                            start: input.current_location(),
                            state: StateType::Delimiter(DelimiterState::default()),
                        });
                        return self.scan_delimiter(input);
                    }
                    ' ' | '\t' => {
                        _ = input.pop();
                        continue;
                    }
                    '@' => {
                        self.set_current_sequence_has_items();

                        _ = input.pop();
                        self.stack.push(State {
                            start: input.current_location(),
                            state: StateType::Tag,
                        });
                        self.current_token.clear();
                        return self.scan_tag(input).map(Some);
                    }
                    '&' => {
                        self.set_current_sequence_has_items();

                        _ = input.pop();
                        self.stack.push(State {
                            start: input.current_location(),
                            state: StateType::Ref,
                        });
                        self.current_token.clear();
                        return self.scan_ref(input).map(Some);
                    }
                    '*' => {
                        self.set_current_sequence_has_items();

                        _ = input.pop();
                        self.stack.push(State {
                            start: input.current_location(),
                            state: StateType::Merge,
                        });
                        self.current_token.clear();
                        return self.scan_merge(input).map(Some);
                    }
                    _ => return self.begin_value(input).map(Some),
                },
            }
        }
    }

    fn begin_value<'r>(
        &'r mut self,
        input: &mut InputBuffer,
    ) -> Result<Spanned<Token<'r>>, Status> {
        self.current_token.clear();

        match input.peek2()? {
            [Input::Value('-'), Input::Value('"')] => {
                self.set_current_sequence_has_items();
                self.stack.push(State {
                    start: input.current_location(),
                    state: StateType::QuotedScalar(TrimMode::Whitespace),
                });
                _ = input.pop();
                _ = input.pop();
                self.scan_quoted(input)
            }
            [Input::Value('>'), Input::Value('"')] => {
                self.set_current_sequence_has_items();
                self.stack.push(State {
                    start: input.current_location(),
                    state: StateType::QuotedScalar(TrimMode::Indentation),
                });
                _ = input.pop();
                _ = input.pop();
                self.scan_quoted(input)
            }
            [Input::Value('"'), _] => {
                self.set_current_sequence_has_items();
                self.stack.push(State {
                    start: input.current_location(),
                    state: StateType::QuotedScalar(TrimMode::None),
                });
                _ = input.pop();
                self.scan_quoted(input)
            }
            [Input::Value('\''), _] => {
                self.set_current_sequence_has_items();
                self.stack.push(State {
                    start: input.current_location(),
                    state: StateType::SingleQuotedScalar,
                });
                _ = input.pop();
                self.scan_quoted(input)
            }
            [Input::Value('{'), _] => {
                self.set_current_sequence_has_items();
                let start = input.current_location();
                self.stack.push(State {
                    start,
                    state: StateType::Sequence(SequenceState::default()),
                });
                _ = input.pop();
                let end = input.current_location();
                Ok(Token::SequenceStartCurly.in_span(start..end))
            }
            [Input::Value('['), _] => {
                self.set_current_sequence_has_items();
                let start = input.current_location();
                self.stack.push(State {
                    start,
                    state: StateType::Sequence(SequenceState::default()),
                });
                _ = input.pop();
                let end = input.current_location();
                Ok(Token::SequenceStartSquare.in_span(start..end))
            }
            [Input::Value('('), _] => {
                self.set_current_sequence_has_items();
                let start = input.current_location();
                self.stack.push(State {
                    start,
                    state: StateType::Sequence(SequenceState::default()),
                });
                _ = input.pop();
                let end = input.current_location();
                Ok(Token::SequenceStartParens.in_span(start..end))
            }
            [Input::Value(ch), _] => {
                if ch.is_whitespace() {
                    panic!("scanner error: unexpected whitespace (should have been skipped before this point)");
                }

                self.set_current_sequence_has_items();
                self.stack.push(State {
                    start: input.current_location(),
                    state: StateType::PlainScalar,
                });
                self.scan_plain(input)
            }
            [Input::Eof, _] => {
                panic!(
                    "scanner error: unexpected eof (should have been detected before this point)"
                )
            }
        }
    }

    fn scan_plain<'r>(&'r mut self, input: &mut InputBuffer) -> Result<Spanned<Token<'r>>, Status> {
        loop {
            match input.peek2()? {
                [Input::Value(':'), Input::Value(':')] => {
                    self.current_token.push_str("::");
                    _ = input.pop();
                    _ = input.pop();
                }
                [Input::Value(ch), _] if ch.is_valid_plain() => {
                    self.current_token.push(ch);
                    _ = input.pop();
                }
                _ => break,
            }
        }

        if self.current_token.is_empty() {
            match input.peek().unwrap() {
                Input::Eof => return Err(ScannerError::UnexpectedEof.into()),
                Input::Value(ch) => return Err(ScannerError::UnexpectedChar(ch).into()),
            }
        }

        let Some(State {
            start,
            state: StateType::PlainScalar,
        }) = self.stack.pop()
        else {
            panic!("inconsistent stack")
        };

        Ok(
            Token::Scalar(Scalar::new(&self.current_token, ScalarStyle::Plain))
                .in_span(start..input.current_location()),
        )
    }

    fn scan_tag<'r>(&'r mut self, input: &mut InputBuffer) -> Result<Spanned<Token<'r>>, Status> {
        self.scan_complete_ident(input)?;
        if self.current_token.is_empty() {
            return Err(ScannerError::UnexpectedChar('@').into());
        }

        let Some(State {
            start,
            state: StateType::Tag,
        }) = self.stack.pop()
        else {
            panic!("inconsistent stack")
        };

        Ok(Token::Anchor(&self.current_token).in_span(start..input.current_location()))
    }

    fn scan_ref<'r>(&'r mut self, input: &mut InputBuffer) -> Result<Spanned<Token<'r>>, Status> {
        self.scan_complete_ident(input)?;
        if self.current_token.is_empty() {
            return Err(ScannerError::UnexpectedChar('&').into());
        }

        let Some(State {
            start,
            state: StateType::Ref,
        }) = self.stack.pop()
        else {
            panic!("inconsistent stack")
        };

        Ok(Token::Ref(&self.current_token).in_span(start..input.current_location()))
    }

    fn scan_merge<'r>(&'r mut self, input: &mut InputBuffer) -> Result<Spanned<Token<'r>>, Status> {
        self.scan_complete_ident(input)?;
        if self.current_token.is_empty() {
            return Err(ScannerError::UnexpectedChar('*').into());
        }

        let Some(State {
            start,
            state: StateType::Merge,
        }) = self.stack.pop()
        else {
            panic!("inconsistent stack")
        };

        Ok(Token::Merge(&self.current_token).in_span(start..input.current_location()))
    }

    fn scan_complete_ident(&mut self, input: &mut InputBuffer) -> Result<(), Status> {
        loop {
            match input.peek()? {
                Input::Value(ch) if ch.is_valid_plain() => {
                    input.pop()?;
                    self.current_token.push(ch);
                }
                _ => return Ok(()),
            }
        }
    }

    fn scan_quoted<'r>(
        &'r mut self,
        input: &mut InputBuffer,
    ) -> Result<Spanned<Token<'r>>, Status> {
        let (end_char, start, style) = match self.stack.last().expect("empty stack") {
            State {
                state: StateType::QuotedScalar(trim_mode),
                start,
            } => (
                '"',
                *start,
                match trim_mode {
                    TrimMode::None => ScalarStyle::Quoted,
                    TrimMode::Whitespace => ScalarStyle::QuotedTrimWhitespace,
                    TrimMode::Indentation => ScalarStyle::QuotedTrimIndent,
                },
            ),
            State {
                state: StateType::SingleQuotedScalar,
                start,
            } => ('\'', *start, ScalarStyle::SingleQuoted),
            _ => panic!("inconsistent stack"),
        };

        loop {
            match input.peek()? {
                Input::Value(ch) if ch == end_char => {
                    _ = input.pop();
                    break;
                }
                Input::Value(ch) => {
                    _ = input.pop();
                    self.current_token.push(ch);
                }
                Input::Eof => {
                    return Err(ScannerError::UnexpectedEof.into());
                }
            }
        }

        self.stack.pop();

        Ok(Token::Scalar(Scalar::new(&self.current_token, style))
            .in_span(start..input.current_location()))
    }

    fn scan_comment<'r>(
        &'r mut self,
        input: &mut InputBuffer,
    ) -> Result<Spanned<Token<'r>>, Status> {
        assert!(matches!(
            self.stack.last(),
            Some(State {
                state: StateType::Comment,
                ..
            })
        ));

        loop {
            match input.peek()? {
                Input::Value('\n') => {
                    _ = input.pop();
                    break;
                }
                Input::Value('\r') => {
                    _ = input.pop();
                    continue;
                }
                Input::Value(ch) => {
                    self.current_token.push(ch);
                    _ = input.pop();
                }
                Input::Eof => {
                    break;
                }
            }
        }

        let Some(State {
            start,
            state: StateType::Comment,
        }) = self.stack.pop()
        else {
            panic!("inconsistent stack")
        };

        Ok(Token::Comment(&self.current_token).in_span(start..input.current_location()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[track_caller]
    fn assert_tokens_eq(input: &str, expected: &[Token]) {
        let mut scanner = Scanner::new(input.as_bytes());
        let mut tokens = vec![];
        loop {
            match scanner.next_token() {
                Ok(Some(token)) => tokens.push(token.value.to_owned()),
                Ok(None) => break,
                Err(e) => panic!("error: {}", e),
            }
        }
        assert_eq!(tokens, expected);
    }

    #[track_caller]
    fn assert_tokens_err(input: &str, expected: ScannerError) {
        let mut scanner = Scanner::new(input.as_bytes());
        let mut tokens = vec![];
        loop {
            match scanner.next_token() {
                Ok(Some(token)) => tokens.push(token.value.to_owned()),
                Ok(None) => break,
                Err(e) => {
                    assert_eq!(e, expected);
                    return;
                }
            }
        }

        panic!("did not get an error, expected {expected:?}")
    }

    #[test]
    fn one_scalar() {
        assert_tokens_eq("hello", &[Token::plain("hello")]);
        assert_tokens_eq(".0", &[Token::plain(".0")]);
        assert_tokens_eq("0.0", &[Token::plain("0.0")]);
    }

    #[test]
    fn one_quoted_scalar() {
        assert_tokens_eq("\"hello\"", &[Token::quoted("hello")]);
        assert_tokens_eq("\"hello world\"", &[Token::quoted("hello world")]);
    }

    #[test]
    fn list_of_plain_scalars() {
        assert_tokens_eq(
            "hello\nworld",
            &[
                Token::plain("hello"),
                Token::Delimiter(Delimiter::Newline),
                Token::plain("world"),
            ],
        );
    }

    #[test]
    fn list_of_quoted_scalars() {
        assert_tokens_eq(
            "\"hello\"\n\"world\"",
            &[
                Token::quoted("hello"),
                Token::Delimiter(Delimiter::Newline),
                Token::quoted("world"),
            ],
        );
    }

    #[test]
    fn comma_delimiter_root() {
        assert_tokens_eq(
            "hello, world!",
            &[
                Token::plain("hello"),
                Token::Delimiter(Delimiter::Comma),
                Token::plain("world!"),
            ],
        );
    }

    #[test]
    fn key_values() {
        assert_tokens_eq(
            "key: value",
            &[Token::plain("key"), Token::Colon, Token::plain("value")],
        );
    }

    #[test]
    fn string_key_values() {
        assert_tokens_eq(
            "\"key\": \"value\"",
            &[Token::quoted("key"), Token::Colon, Token::quoted("value")],
        );
    }

    #[test]
    fn nested_sequences_square() {
        assert_tokens_eq(
            "[1, 2, 3]\n[4, 5, 6]",
            &[
                Token::SequenceStartSquare,
                Token::plain("1"),
                Token::Delimiter(Delimiter::Comma),
                Token::plain("2"),
                Token::Delimiter(Delimiter::Comma),
                Token::plain("3"),
                Token::SequenceEndSquare,
                Token::Delimiter(Delimiter::Newline),
                Token::SequenceStartSquare,
                Token::plain("4"),
                Token::Delimiter(Delimiter::Comma),
                Token::plain("5"),
                Token::Delimiter(Delimiter::Comma),
                Token::plain("6"),
                Token::SequenceEndSquare,
            ],
        );
    }

    #[test]
    fn nested_sequences_curly() {
        assert_tokens_eq(
            "{1, 2, 3}\n{4, 5, 6}",
            &[
                Token::SequenceStartCurly,
                Token::plain("1"),
                Token::Delimiter(Delimiter::Comma),
                Token::plain("2"),
                Token::Delimiter(Delimiter::Comma),
                Token::plain("3"),
                Token::SequenceEndCurly,
                Token::Delimiter(Delimiter::Newline),
                Token::SequenceStartCurly,
                Token::plain("4"),
                Token::Delimiter(Delimiter::Comma),
                Token::plain("5"),
                Token::Delimiter(Delimiter::Comma),
                Token::plain("6"),
                Token::SequenceEndCurly,
            ],
        );
    }

    #[test]
    fn list_of_floats() {
        assert_tokens_eq(
            "1.0\n2.0\n3.0",
            &[
                Token::plain("1.0"),
                Token::Delimiter(Delimiter::Newline),
                Token::plain("2.0"),
                Token::Delimiter(Delimiter::Newline),
                Token::plain("3.0"),
            ],
        );
    }

    #[test]
    fn skip_initial_newlines() {
        assert_tokens_eq(
            "\n\n\nhello",
            &[Token::Scalar(Scalar::new("hello", ScalarStyle::Plain))],
        );
    }

    #[test]
    fn initial_comma() {
        assert_tokens_err(",hello", ScannerError::UnexpectedChar(','))
    }

    #[test]
    fn initial_colon() {
        assert_tokens_err(":hello", ScannerError::UnexpectedChar(':'))
    }

    #[test]
    fn initial_at_sign() {
        assert_tokens_err("@ hello", ScannerError::UnexpectedChar('@'));
    }

    #[test]
    fn initial_ampersand() {
        assert_tokens_err("& hello", ScannerError::UnexpectedChar('&'));
    }

    #[test]
    fn initial_asterisk() {
        assert_tokens_err("* hello", ScannerError::UnexpectedChar('*'));
    }

    #[test]
    fn tag() {
        assert_tokens_eq("@hello", &[Token::Anchor("hello")]);
    }

    #[test]
    fn tagged_value() {
        assert_tokens_eq(
            "@hello [1, 2, 3]",
            &[
                Token::Anchor("hello"),
                Token::SequenceStartSquare,
                Token::plain("1"),
                Token::Delimiter(Delimiter::Comma),
                Token::plain("2"),
                Token::Delimiter(Delimiter::Comma),
                Token::plain("3"),
                Token::SequenceEndSquare,
            ],
        )
    }

    #[test]
    fn value_reference() {
        assert_tokens_eq(
            "[&hello, *hello, 3]",
            &[
                Token::SequenceStartSquare,
                Token::Ref("hello"),
                Token::Delimiter(Delimiter::Comma),
                Token::Merge("hello"),
                Token::Delimiter(Delimiter::Comma),
                Token::Scalar(Scalar::new("3", ScalarStyle::Plain)),
                Token::SequenceEndSquare,
            ],
        )
    }

    #[test]
    fn comments() {
        assert_tokens_eq("# hello, world!", &[Token::Comment(" hello, world!")])
    }

    #[test]
    fn double_colon_is_plain_scalar() {
        assert_tokens_eq("::", &[Token::plain("::")]);
        assert_tokens_eq(
            ":::hello",
            &[Token::plain("::"), Token::Colon, Token::plain("hello")],
        );
        assert_tokens_eq("::::hello", &[Token::plain("::::hello")]);
    }

    #[test]
    fn punctuation_is_identifier() {
        assert_tokens_eq("/", &[Token::Scalar(Scalar::new("/", ScalarStyle::Plain))]);
        assert_tokens_eq(
            "\\",
            &[Token::Scalar(Scalar::new("\\", ScalarStyle::Plain))],
        );
        assert_tokens_eq(".", &[Token::plain(".")]);
        assert_tokens_eq("!", &[Token::plain("!")]);
        assert_tokens_eq("=", &[Token::plain("=")]);
        assert_tokens_eq("-", &[Token::plain("-")]);
        assert_tokens_eq("+", &[Token::plain("+")]);
    }

    #[test]
    fn quotes_separate_scalars() {
        assert_tokens_eq(
            "hello\"world\"",
            &[Token::plain("hello"), Token::quoted("world")],
        )
    }
}
