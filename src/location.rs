#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct SourceLocation {
    /// Offset in Unicode chars from the beginning of the byte stream.
    pub offset: usize,
    /// Line, counting from 0.
    pub line: usize,
    /// Column, counting from 0.
    pub column: usize,
}

impl PartialOrd for SourceLocation {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for SourceLocation {
    #[inline]
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.offset.cmp(&other.offset)
    }
}

impl SourceLocation {
    #[inline]
    pub fn advance(&mut self, ch: char) {
        self.offset += 1;
        if ch == '\n' {
            self.line += 1;
            self.column = 0;
        } else if ch != '\r' {
            self.column += 1;
        }
    }

    #[inline]
    pub fn until(self, end: Self) -> Span {
        Span { start: self, end }
    }
}

impl std::fmt::Display for SourceLocation {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line + 1, self.column + 1)
    }
}

#[derive(Debug, Clone, Copy, Hash)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn into_inner(self) -> T {
        self.value
    }

    pub fn map<F: FnOnce(T) -> U, U>(self, f: F) -> Spanned<U> {
        Spanned {
            value: f(self.value),
            span: self.span,
        }
    }

    #[inline]
    pub fn as_ref(&self) -> Spanned<&T> {
        Spanned {
            value: &self.value,
            span: self.span,
        }
    }
}

impl<T> std::ops::Deref for Spanned<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> std::ops::DerefMut for Spanned<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

impl<T> PartialEq for Spanned<T>
where
    T: PartialEq,
{
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl PartialEq<str> for Spanned<String> {
    #[inline]
    fn eq(&self, other: &str) -> bool {
        self.value == other
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    pub start: SourceLocation,
    pub end: SourceLocation,
}

impl From<std::ops::Range<SourceLocation>> for Span {
    fn from(range: std::ops::Range<SourceLocation>) -> Self {
        Self {
            start: range.start,
            end: range.end,
        }
    }
}

impl Span {
    #[inline]
    pub fn empty(location: SourceLocation) -> Self {
        Self {
            start: location,
            end: location,
        }
    }

    #[inline]
    pub fn merge(&self, other: &Self) -> Self {
        Self {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

pub(crate) trait SpannedExt {
    fn in_span<R: Into<Span>>(self, span: R) -> Spanned<Self>
    where
        Self: Sized,
    {
        Spanned {
            value: self,
            span: span.into(),
        }
    }
}

impl<T> SpannedExt for T {}
