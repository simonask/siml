use crate::{OwnedScalar, Scalar, ScalarStyle, Span, Spanned};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Token<'r> {
    /// A plain, unquoted scalar. Never needs escaping.
    Scalar(Scalar<'r>),
    /// The `{` token.
    SequenceStartCurly,
    /// The `}` token.
    SequenceEndCurly,
    /// The `[` token.
    SequenceStartSquare,
    /// The `]` token.
    SequenceEndSquare,
    /// The `(` token.
    SequenceStartParens,
    /// The `)` token.
    SequenceEndParens,
    /// A delimiter token (comma, newline, or colon).
    Delimiter(Delimiter),
    /// The '?' token, indicating a complex key. These can only occur in
    /// curly-bracket sequences.
    KeySigil,
    /// The ':' token, separating keys from values. These can only occur in
    /// curly-bracket sequences.
    Colon,
    /// The `@` token. Must be followed by an `Ident`.
    Anchor(&'r str),
    /// The `&` token. Must be followed by an `Ident`.
    Ref(&'r str),
    /// The `*` token. Must be followed by an `Ident`.
    Merge(&'r str),
    /// A comment line (without the leading `#` character).
    Comment(&'r str),
}

impl<'r> Token<'r> {
    #[inline]
    pub fn scalar(value: &'r str, style: ScalarStyle) -> Self {
        Self::Scalar(Scalar::new(value, style))
    }

    #[inline]
    pub fn plain(value: &'r str) -> Self {
        Self::scalar(value, ScalarStyle::Plain)
    }

    #[inline]
    pub fn quoted(value: &'r str) -> Self {
        Self::scalar(value, ScalarStyle::Quoted)
    }
}

impl<'r> std::fmt::Display for Token<'r> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use std::fmt::Write;
        match self {
            Token::Scalar(Scalar {
                value,
                style: ScalarStyle::Plain,
                ..
            }) => write!(f, "{}", value),
            Token::Scalar(Scalar {
                value,
                style: ScalarStyle::Quoted,
                ..
            }) => write!(f, "\"{}\"", value.escape_default()),
            Token::Scalar(Scalar {
                value,
                style: ScalarStyle::QuotedTrimWhitespace,
                ..
            }) => {
                write!(f, "-\"{}\"", value.escape_default())
            }
            Token::Scalar(Scalar {
                value,
                style: ScalarStyle::QuotedTrimIndent,
                ..
            }) => write!(f, ">\"{}\"", value.escape_default()),
            Token::Scalar(Scalar {
                value,
                style: ScalarStyle::SingleQuoted,
                ..
            }) => write!(f, "'{}'", value),
            Token::SequenceStartCurly => f.write_char('{'),
            Token::SequenceEndCurly => f.write_char('}'),
            Token::SequenceStartSquare => f.write_char('['),
            Token::SequenceEndSquare => f.write_char(']'),
            Token::SequenceStartParens => f.write_char('('),
            Token::SequenceEndParens => f.write_char(')'),
            Token::Delimiter(d) => f.write_char(d.as_char()),
            Token::KeySigil => f.write_char('?'),
            Token::Colon => f.write_char(':'),
            Token::Anchor(anchor) => write!(f, "@{anchor}"),
            Token::Ref(anchor) => write!(f, "&{anchor}"),
            Token::Merge(anchor) => write!(f, "*{anchor}"),
            Token::Comment(_) => f.write_char('#'),
        }
    }
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::Scalar(ScalarStyle::Plain) => f.write_str("plain scalar"),
            TokenType::Scalar(ScalarStyle::Quoted) => f.write_str("quoted scalar"),
            TokenType::Scalar(ScalarStyle::QuotedTrimWhitespace) => {
                f.write_str("quoted scalar with trimmed whitespace")
            }
            TokenType::Scalar(ScalarStyle::QuotedTrimIndent) => {
                f.write_str("quoted scalar with trimmed indentation")
            }
            TokenType::Scalar(ScalarStyle::SingleQuoted) => f.write_str("single-quoted scalar"),
            TokenType::SequenceStartCurly => f.write_str("curly sequence start"),
            TokenType::SequenceEndCurly => f.write_str("curly sequence end"),
            TokenType::SequenceStartSquare => f.write_str("square sequence start"),
            TokenType::SequenceEndSquare => f.write_str("square sequence end"),
            TokenType::SequenceStartParens => f.write_str("paren sequence start"),
            TokenType::SequenceEndParens => f.write_str("paren sequence end"),
            TokenType::Delimiter(d) => write!(f, "delimiter '{}'", d.as_char()),
            TokenType::Colon => f.write_str("colon"),
            TokenType::KeySigil => f.write_str("question mark"),
            TokenType::Anchor => f.write_str("tag"),
            TokenType::Ref => f.write_str("reference"),
            TokenType::Merge => f.write_str("merge"),
            TokenType::Comment => f.write_str("comment"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Delimiter {
    /// A comma delimiter. These are always emitted, but are optional in
    /// curly-bracket sequences.
    Comma = ',' as isize,
    /// A newline delimiter. These are emitted in a context-sensitive way, i.e.
    /// within curly-bracket sequences.
    Newline = '\n' as isize,
}

impl Delimiter {
    pub fn as_char(self) -> char {
        match self {
            Delimiter::Comma => ',',
            Delimiter::Newline => '\n',
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum OwnedToken {
    Scalar(OwnedScalar),
    SequenceStartCurly,
    SequenceEndCurly,
    SequenceStartSquare,
    SequenceEndSquare,
    SequenceStartParens,
    SequenceEndParens,
    KeySigil,
    Colon,
    Delimiter(Delimiter),
    Anchor(String),
    Ref(String),
    Merge(String),
    Comment(String),
}

impl std::fmt::Display for OwnedToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.borrow(), f)
    }
}

impl<'r> Token<'r> {
    pub fn to_owned(self) -> OwnedToken {
        match self {
            Token::Scalar(s) => OwnedToken::Scalar(s.to_owned()),
            Token::SequenceStartCurly => OwnedToken::SequenceStartCurly,
            Token::SequenceEndCurly => OwnedToken::SequenceEndCurly,
            Token::SequenceStartSquare => OwnedToken::SequenceStartSquare,
            Token::SequenceEndSquare => OwnedToken::SequenceEndSquare,
            Token::SequenceStartParens => OwnedToken::SequenceStartParens,
            Token::SequenceEndParens => OwnedToken::SequenceEndParens,
            Token::Delimiter(d) => OwnedToken::Delimiter(d),
            Token::KeySigil => OwnedToken::KeySigil,
            Token::Colon => OwnedToken::Colon,
            Token::Anchor(anchor) => OwnedToken::Anchor(anchor.to_owned()),
            Token::Ref(anchor) => OwnedToken::Ref(anchor.to_owned()),
            Token::Merge(anchor) => OwnedToken::Merge(anchor.to_owned()),
            Token::Comment(comment) => OwnedToken::Comment(comment.to_owned()),
        }
    }

    pub fn ty(&self) -> TokenType {
        match self {
            Token::Scalar(s) => TokenType::Scalar(s.style),
            Token::SequenceStartCurly => TokenType::SequenceStartCurly,
            Token::SequenceEndCurly => TokenType::SequenceEndCurly,
            Token::SequenceStartSquare => TokenType::SequenceStartSquare,
            Token::SequenceEndSquare => TokenType::SequenceEndSquare,
            Token::SequenceStartParens => TokenType::SequenceStartParens,
            Token::SequenceEndParens => TokenType::SequenceEndParens,
            Token::Colon => TokenType::Colon,
            Token::KeySigil => TokenType::KeySigil,
            Token::Delimiter(d) => TokenType::Delimiter(*d),
            Token::Anchor(_) => TokenType::Anchor,
            Token::Ref(_) => TokenType::Ref,
            Token::Merge(_) => TokenType::Merge,
            Token::Comment(_) => TokenType::Comment,
        }
    }
}

impl OwnedToken {
    pub fn borrow(&self) -> Token {
        match self {
            OwnedToken::Scalar(s) => Token::Scalar(s.borrow()),
            OwnedToken::SequenceStartCurly => Token::SequenceStartCurly,
            OwnedToken::SequenceEndCurly => Token::SequenceEndCurly,
            OwnedToken::SequenceStartSquare => Token::SequenceStartSquare,
            OwnedToken::SequenceEndSquare => Token::SequenceEndSquare,
            OwnedToken::SequenceStartParens => Token::SequenceStartParens,
            OwnedToken::SequenceEndParens => Token::SequenceEndParens,
            OwnedToken::Colon => Token::Colon,
            OwnedToken::KeySigil => Token::KeySigil,
            OwnedToken::Delimiter(d) => Token::Delimiter(*d),
            OwnedToken::Anchor(anchor) => Token::Anchor(anchor),
            OwnedToken::Ref(anchor) => Token::Ref(anchor),
            OwnedToken::Merge(anchor) => Token::Merge(anchor),
            OwnedToken::Comment(comment) => Token::Comment(comment),
        }
    }
}

impl PartialEq<Token<'_>> for OwnedToken {
    fn eq(&self, other: &Token) -> bool {
        self.borrow() == *other
    }
}

/// A cached token used by parser lookahead. This efficiently handles the
/// strings of the cached token, such that allocations are amortized away.
#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct CachedToken {
    ty: Option<TokenType>,
    value: String,
    span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenType {
    Scalar(ScalarStyle),
    SequenceStartCurly,
    SequenceEndCurly,
    SequenceStartSquare,
    SequenceEndSquare,
    SequenceStartParens,
    SequenceEndParens,
    Colon,
    KeySigil,
    Delimiter(Delimiter),
    Anchor,
    Ref,
    Merge,
    Comment,
}

impl CachedToken {
    pub fn set(&mut self, token: Option<Spanned<Token>>) {
        self.value.clear();

        if let Some(token) = token {
            self.span = token.span;
            let (ty, value) = match token.value {
                Token::Scalar(Scalar { style, value, .. }) => (TokenType::Scalar(style), value),
                Token::SequenceStartCurly => (TokenType::SequenceStartCurly, ""),
                Token::SequenceEndCurly => (TokenType::SequenceEndCurly, ""),
                Token::SequenceStartSquare => (TokenType::SequenceStartSquare, ""),
                Token::SequenceEndSquare => (TokenType::SequenceEndSquare, ""),
                Token::SequenceStartParens => (TokenType::SequenceStartParens, ""),
                Token::SequenceEndParens => (TokenType::SequenceEndParens, ""),
                Token::Colon => (TokenType::Colon, ""),
                Token::KeySigil => (TokenType::KeySigil, ""),
                Token::Delimiter(d) => (TokenType::Delimiter(d), ""),
                Token::Anchor(value) => (TokenType::Anchor, value),
                Token::Ref(value) => (TokenType::Ref, value),
                Token::Merge(value) => (TokenType::Merge, value),
                Token::Comment(value) => (TokenType::Comment, value),
            };
            self.ty = Some(ty);
            self.value.push_str(value);
        } else {
            self.ty = None;
        }
    }

    pub fn get(&self) -> Option<(Token, Span)> {
        Some((
            match self.ty? {
                TokenType::Scalar(style) => Token::Scalar(Scalar {
                    value: &self.value,
                    style,
                    anchor: None,
                }),
                TokenType::SequenceStartCurly => Token::SequenceStartCurly,
                TokenType::SequenceEndCurly => Token::SequenceEndCurly,
                TokenType::SequenceStartSquare => Token::SequenceStartSquare,
                TokenType::SequenceEndSquare => Token::SequenceEndSquare,
                TokenType::SequenceStartParens => Token::SequenceStartParens,
                TokenType::SequenceEndParens => Token::SequenceEndParens,
                TokenType::Colon => Token::Colon,
                TokenType::KeySigil => Token::KeySigil,
                TokenType::Delimiter(d) => Token::Delimiter(d),
                TokenType::Anchor => Token::Anchor(&self.value),
                TokenType::Ref => Token::Ref(&self.value),
                TokenType::Merge => Token::Merge(&self.value),
                TokenType::Comment => Token::Comment(&self.value),
            },
            self.span,
        ))
    }
}
