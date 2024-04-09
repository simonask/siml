use std::io::BufRead;

use crate::{
    error::Error, CachedToken, CharExt, Delimiter, Input, InputBuffer, NeedMore, Scalar,
    ScalarStyle, SourceLocation, SpannedExt, StringExt as _, Token,
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
    trim_indent_buffer: String,
    root_has_items: bool,
}

#[derive(Debug, thiserror::Error, PartialEq)]
pub enum ScannerError {
    #[error("unexpected char: {0:?}")]
    UnexpectedChar(char),
    #[error("invalid escape sequence: {0:?}")]
    InvalidEscapeSequence(char),
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
    QuotedScalar,
    SingleQuotedScalar,
    QuotedScalarTrimWhitespace,
    QuotedScalarTrimIndent,
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

impl<R: BufRead> Scanner<R> {
    pub fn new(reader: R) -> Self {
        Self {
            reader,
            buffer: InputBuffer::default(),
            inner: ScannerInner::default(),
            eof: false,
        }
    }

    /// Read the next token from the reader into `out`.
    ///
    /// When `out.is_some()` is `true`, and no error occurred, a token was
    /// successfully read. When `out.is_some()` is `false`, and no error
    /// occurred, the EOF was reached.
    pub fn next_token(&mut self, out: &mut CachedToken) -> Result<(), Error> {
        let Self {
            reader,
            buffer,
            inner,
            eof,
        } = self;

        loop {
            out.clear();
            match inner.next_token(buffer, out) {
                Ok(()) => return Ok(()),
                Err(Status::NeedMore(NeedMore(at_least))) => {
                    match Self::read_chars(reader, buffer, at_least, eof) {
                        Ok(()) => continue,
                        Err(e) => return Err(e),
                    }
                }
                Err(Status::Error(e)) => return Err(e),
            }
        }
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
    fn next_token(&mut self, input: &mut InputBuffer, out: &mut CachedToken) -> Result<(), Status> {
        let Some(state) = self.stack.last() else {
            return self.scan_sequence(input, out);
        };

        match state.state {
            StateType::Delimiter(_) => self.scan_delimiter(input, out),
            StateType::Sequence(_) => self.scan_sequence(input, out),
            StateType::PlainScalar => self.scan_plain(input, out),
            StateType::QuotedScalar
            | StateType::SingleQuotedScalar
            | StateType::QuotedScalarTrimWhitespace
            | StateType::QuotedScalarTrimIndent { .. } => self.scan_quoted(input, out),
            StateType::Tag => self.scan_tag(input, out),
            StateType::Ref => self.scan_ref(input, out),
            StateType::Merge => self.scan_merge(input, out),
            StateType::Comment => self.scan_comment(input, out),
        }
    }

    fn scan_delimiter(
        &mut self,
        input: &mut InputBuffer,
        out: &mut CachedToken,
    ) -> Result<(), Status> {
        let Some(State {
            state: StateType::Delimiter(state),
            ..
        }) = self.stack.last_mut()
        else {
            panic!("inconsistent scanner state (rule: delimiter)");
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
                    return self.scan_comment(input, out);
                }
                _ => break,
            }
        }

        let Some(State {
            state: StateType::Delimiter(state),
            start,
        }) = self.stack.pop()
        else {
            panic!("inconsistent scanner state (rule: delimiter)");
        };
        let end = input.current_location();

        if state.comma {
            out.set(Some(Token::Delimiter(Delimiter::Comma).in_span(start..end)));
            return Ok(());
        }
        if state.newlines > 0 {
            out.set(Some(
                Token::Delimiter(Delimiter::Newline).in_span(start..end),
            ));
            return Ok(());
        }
        if input.is_eof() {
            return Ok(());
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
            _ => panic!("inconsistent scanner state (rule: sequence)"),
        }
    }

    fn scan_sequence(
        &mut self,
        input: &mut InputBuffer,
        out: &mut CachedToken,
    ) -> Result<(), Status> {
        let is_root = self.stack.is_empty();

        let is_initial = match self.stack.last() {
            Some(State {
                state: StateType::Sequence(state),
                ..
            }) => !state.has_items,
            None => !self.root_has_items,
            _ => panic!("inconsistent scanner state (rule: sequence)"),
        };

        loop {
            match input.peek()? {
                Input::Eof if is_root => {
                    return Ok(());
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

                        out.set(Some(Token::SequenceEndCurly.in_span(start..end)));
                        return Ok(());
                    }
                    ']' => {
                        self.stack.pop().expect("empty stack");

                        let start = input.current_location();
                        _ = input.pop();
                        let end = input.current_location();

                        out.set(Some(Token::SequenceEndSquare.in_span(start..end)));
                        return Ok(());
                    }
                    ')' => {
                        self.stack.pop().expect("empty stack");
                        let start = input.current_location();
                        _ = input.pop();
                        let end = input.current_location();

                        out.set(Some(Token::SequenceEndParens.in_span(start..end)));
                        return Ok(());
                    }
                    '#' => {
                        _ = input.pop();
                        self.stack.push(State {
                            start: input.current_location(),
                            state: StateType::Comment,
                        });
                        self.current_token.clear();
                        return self.scan_comment(input, out);
                    }
                    '?' => {
                        let start = input.current_location();
                        _ = input.pop();
                        let end = input.current_location();
                        out.set(Some(Token::KeySigil.in_span(start..end)));
                        return Ok(());
                    }
                    ':' if !is_initial => {
                        let start = input.current_location();
                        _ = input.pop();
                        let end = input.current_location();
                        out.set(Some(Token::Colon.in_span(start..end)));
                        return Ok(());
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
                        return self.scan_delimiter(input, out);
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
                        return self.scan_tag(input, out);
                    }
                    '&' => {
                        self.set_current_sequence_has_items();

                        _ = input.pop();
                        self.stack.push(State {
                            start: input.current_location(),
                            state: StateType::Ref,
                        });
                        self.current_token.clear();
                        return self.scan_ref(input, out);
                    }
                    '*' => {
                        self.set_current_sequence_has_items();

                        _ = input.pop();
                        self.stack.push(State {
                            start: input.current_location(),
                            state: StateType::Merge,
                        });
                        self.current_token.clear();
                        return self.scan_merge(input, out);
                    }
                    _ => return self.begin_value(input, out),
                },
            }
        }
    }

    fn begin_value(
        &mut self,
        input: &mut InputBuffer,
        token: &mut CachedToken,
    ) -> Result<(), Status> {
        self.current_token.clear();

        match input.peek2()? {
            [Input::Value('-'), Input::Value('"')] => {
                self.set_current_sequence_has_items();
                self.stack.push(State {
                    start: input.current_location(),
                    state: StateType::QuotedScalarTrimWhitespace,
                });
                _ = input.pop2();
                self.scan_quoted(input, token)
            }
            [Input::Value('|'), Input::Value('"')] => {
                self.set_current_sequence_has_items();
                self.stack.push(State {
                    start: input.current_location(),
                    state: StateType::QuotedScalarTrimIndent,
                });
                _ = input.pop2();
                self.scan_quoted(input, token)
            }
            [Input::Value('"'), _] => {
                self.set_current_sequence_has_items();
                self.stack.push(State {
                    start: input.current_location(),
                    state: StateType::QuotedScalar,
                });
                _ = input.pop();
                self.scan_quoted(input, token)
            }
            [Input::Value('\''), _] => {
                self.set_current_sequence_has_items();
                self.stack.push(State {
                    start: input.current_location(),
                    state: StateType::SingleQuotedScalar,
                });
                _ = input.pop();
                self.scan_quoted(input, token)
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
                token.set(Some(Token::SequenceStartCurly.in_span(start..end)));
                return Ok(());
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
                token.set(Some(Token::SequenceStartSquare.in_span(start..end)));
                return Ok(());
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
                token.set(Some(Token::SequenceStartParens.in_span(start..end)));
                Ok(())
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
                self.scan_plain(input, token)
            }
            [Input::Eof, _] => {
                panic!(
                    "scanner error: unexpected eof (should have been detected before this point)"
                )
            }
        }
    }

    fn scan_plain(
        &mut self,
        input: &mut InputBuffer,
        token: &mut CachedToken,
    ) -> Result<(), Status> {
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
            panic!("inconsistent stack (rule: plain scalar)")
        };

        token.set(Some(
            Token::Scalar(Scalar::new(&self.current_token, ScalarStyle::Plain))
                .in_span(start..input.current_location()),
        ));
        Ok(())
    }

    fn scan_tag(&mut self, input: &mut InputBuffer, token: &mut CachedToken) -> Result<(), Status> {
        self.scan_complete_ident(input)?;
        if self.current_token.is_empty() {
            return Err(ScannerError::UnexpectedChar('@').into());
        }

        let Some(State {
            start,
            state: StateType::Tag,
        }) = self.stack.pop()
        else {
            panic!("inconsistent stack (rule: tag)")
        };

        token.set(Some(
            Token::Anchor(&self.current_token).in_span(start..input.current_location()),
        ));
        Ok(())
    }

    fn scan_ref(&mut self, input: &mut InputBuffer, token: &mut CachedToken) -> Result<(), Status> {
        self.scan_complete_ident(input)?;
        if self.current_token.is_empty() {
            return Err(ScannerError::UnexpectedChar('&').into());
        }

        let Some(State {
            start,
            state: StateType::Ref,
        }) = self.stack.pop()
        else {
            panic!("inconsistent stack (rule: ref)")
        };

        token.set(Some(
            Token::Ref(&self.current_token).in_span(start..input.current_location()),
        ));
        Ok(())
    }

    fn scan_merge(
        &mut self,
        input: &mut InputBuffer,
        token: &mut CachedToken,
    ) -> Result<(), Status> {
        self.scan_complete_ident(input)?;
        if self.current_token.is_empty() {
            return Err(ScannerError::UnexpectedChar('*').into());
        }

        let Some(State {
            start,
            state: StateType::Merge,
        }) = self.stack.pop()
        else {
            panic!("inconsistent stack (rule: merge)")
        };

        token.set(Some(
            Token::Merge(&self.current_token).in_span(start..input.current_location()),
        ));
        Ok(())
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

    fn scan_quoted(
        &mut self,
        input: &mut InputBuffer,
        token: &mut CachedToken,
    ) -> Result<(), Status> {
        let (state, start) = if let State {
            state:
                state @ (StateType::QuotedScalar
                | StateType::QuotedScalarTrimWhitespace
                | StateType::QuotedScalarTrimIndent
                | StateType::SingleQuotedScalar),
            start,
        } = self.stack.last().expect("empty stack")
        {
            (*state, *start)
        } else {
            panic!("inconsistent stack (rule: quoted)")
        };

        let end_char = if let StateType::SingleQuotedScalar = state {
            '\''
        } else {
            '"'
        };

        loop {
            match input.peek2()? {
                [Input::Value('\\'), Input::Value(ch)] => {
                    let unescaped = match ch {
                        '"' => '"',
                        '\'' => '\'',
                        '0' => '\0',
                        'a' => '\x07',
                        'b' => '\x08',
                        't' => '\t',
                        'n' => '\n',
                        'v' => '\x0b',
                        'f' => '\x0c',
                        'r' => '\r',
                        'u' => {
                            todo!()
                        }
                        ch => return Err(ScannerError::InvalidEscapeSequence(ch).into()),
                    };
                    self.current_token.push(unescaped);
                    _ = input.pop2();
                }
                [Input::Value('"'), _] if end_char == '"' => {
                    _ = input.pop();
                    break;
                }
                [Input::Value('\''), _] if end_char == '\'' => {
                    _ = input.pop();
                    break;
                }
                [Input::Value(_), Input::Eof] => {
                    return Err(ScannerError::UnexpectedEof.into());
                }
                // Ignore unescaped '\r'.
                [Input::Value('\r'), _] => {
                    _ = input.pop();
                    continue;
                }
                [Input::Value(ch), Input::Value(_)] => {
                    self.current_token.push(ch);
                    _ = input.pop();
                }
                [Input::Eof, _] => {
                    return Err(ScannerError::UnexpectedEof.into());
                }
            }
        }

        self.stack.pop();

        let scalar_token = match state {
            StateType::QuotedScalar => {
                Token::Scalar(Scalar::new(&self.current_token, ScalarStyle::Quoted))
            }
            StateType::SingleQuotedScalar => {
                Token::Scalar(Scalar::new(&self.current_token, ScalarStyle::SingleQuoted))
            }
            StateType::QuotedScalarTrimWhitespace => Token::Scalar(Scalar::new(
                self.current_token.trim(),
                ScalarStyle::QuotedTrimWhitespace,
            )),
            StateType::QuotedScalarTrimIndent => {
                self.current_token
                    .trim_indent_into(&mut self.trim_indent_buffer);
                Token::Scalar(Scalar::new(
                    &self.trim_indent_buffer,
                    ScalarStyle::QuotedTrimIndent,
                ))
            }
            _ => unreachable!(),
        }
        .in_span(start..input.current_location());
        token.set(Some(scalar_token));
        Ok(())
    }

    fn scan_comment(
        &mut self,
        input: &mut InputBuffer,
        token: &mut CachedToken,
    ) -> Result<(), Status> {
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
            panic!("inconsistent stack (rule: comment)")
        };

        token.set(Some(
            Token::Comment(&self.current_token).in_span(start..input.current_location()),
        ));
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[track_caller]
    fn assert_tokens_eq(input: &str, expected: &[Token]) {
        let mut scanner = Scanner::new(input.as_bytes());
        let mut tokens = vec![];
        let mut cached = CachedToken::default();
        loop {
            match scanner.next_token(&mut cached) {
                Ok(_) => {
                    if cached.is_some() {
                        tokens.push(std::mem::take(&mut cached))
                    } else {
                        break;
                    }
                }
                Err(e) => panic!("error: {}", e),
            }
        }
        assert_eq!(tokens, expected);
    }

    #[track_caller]
    fn assert_tokens_err(input: &str, expected: ScannerError) {
        let mut scanner = Scanner::new(input.as_bytes());
        let mut tokens = vec![];
        let mut cached = CachedToken::default();
        loop {
            match scanner.next_token(&mut cached) {
                Ok(_) => {
                    if cached.is_some() {
                        tokens.push(std::mem::take(&mut cached))
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
