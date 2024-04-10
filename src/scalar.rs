use crate::CharExt;

/// Any string-like value.
///
/// The parser does not distinguish between strings and numbers.
#[derive(Debug, Clone, Copy, Eq)]
pub struct Scalar<'r> {
    /// The parsed string value.
    pub value: &'r str,
    /// For a parsed scalar, this is the style that it was parsed with. When
    /// emitting a scalar, this is considered the preferred style, but depending
    /// on the value, it may be emitted using a more appropriate style.
    ///
    /// Note that the style is ignored when comparing scalars.
    pub style: ScalarStyle,
    /// The anchor of the scalar. Note that anchors are not valid for all
    /// scalars, such as the "type tags" of sequences, and will be ignored by
    /// the emitter in such places.
    pub anchor: Option<&'r str>,
}

impl<'r> std::hash::Hash for Scalar<'r> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.value.hash(state);
    }
}

impl<'r> Ord for Scalar<'r> {
    #[inline]
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.value.cmp(other.value)
    }
}

impl<'r> PartialOrd for Scalar<'r> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.value.partial_cmp(other.value)
    }
}

impl<'r> PartialEq for Scalar<'r> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value && self.style == other.style
    }
}

impl<'r> std::ops::Deref for Scalar<'r> {
    type Target = str;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.value
    }
}

bitflags::bitflags! {
    #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct ScalarFlags: u8 {
        const HAS_NEWLINES = 0b0000_0001;
        const HAS_QUOTES = 0b0000_0010;
        const HAS_ESCAPES = 0b0000_0100;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ScalarStyle {
    /// Identifiers or plain numbers, without whitespace or escape sequences.
    ///
    /// Valid characters in a plain scalar:
    ///
    /// - All alphanumeric characters (where `std::char::is_alphanumeric()`
    ///   returns true). These include alphanumeric Unicode characters.
    /// - The following ASCII punctuation characters: `.!/\=-+_`.
    /// - Graphical Unicode sequences, like emojis.
    ///
    /// Plain scalars are guaranteed to always be zero-copy readable by the
    /// parser, because the input can be viewed as a plain scalar verbatim.
    Plain,
    /// A string in double quotes. These may contain whitespace and escape
    /// sequences.
    Quoted,
    /// A string in single quotes. These may contain whitespace and escape
    /// sequences.
    SingleQuoted,
    /// A string in quotes, where initial and trailing whitespace has been
    /// trimmed with the same rules as [`std::str::trim()`], which includes all
    /// whitespace characters defined by Unicode.
    QuotedTrimWhitespace,
    /// A string in quotes, where initial and trailing whitespace is removed,
    /// and each subsequent line after the first has initial whitespace
    /// identical to the leading whitespace of the first line removed.
    ///
    /// Newlines inside the string are preserved.
    ///
    /// This is useful for preserving the indentation of a multiline string.
    ///
    /// ```siml
    /// my_long_string: |"
    ///     This is a long string,
    ///     with multiple lines.
    /// "
    /// # ... is equal to
    /// my_long_string: "This is a long string,\nwith multiple lines."
    /// ```
    QuotedTrimIndent,
    // TODO: Folded scalars?
}

impl ScalarStyle {
    /// Analyze the string and determine an appropriate style for the scalar.
    pub fn infer(value: &str) -> ScalarStyle {
        let mut contains_whitespace = false;
        let mut contains_characters_that_must_be_escaped = false;
        let mut contains_non_plain_characters = false;
        let mut contains_lines_ending_in_whitespace = false;
        let mut contains_dquote = false;
        let mut contains_squote = false;
        let mut num_lines = 0;
        let mut longest_line_length = 0;
        let leading_whitespace = value.starts_with(char::is_whitespace);
        let trailing_whitespace = value.ends_with(char::is_whitespace);

        for line in value.lines() {
            num_lines += 1;
            longest_line_length = longest_line_length.max(line.len());
            contains_lines_ending_in_whitespace |= line.ends_with(char::is_whitespace);
            for ch in line.chars() {
                contains_whitespace |= ch.is_whitespace();
                contains_non_plain_characters |= !ch.is_valid_plain();
                // TODO: We probably need to escape more than just this?
                contains_characters_that_must_be_escaped |= ch.is_control();
                contains_dquote |= ch == '"';
                contains_squote |= ch == '\'';
            }
        }

        let forbid_plain = contains_whitespace
            || contains_non_plain_characters
            || contains_characters_that_must_be_escaped
            || contains_squote
            || contains_dquote
            || num_lines != 1;

        if !forbid_plain {
            return ScalarStyle::Plain;
        }

        if num_lines > 1
            && longest_line_length > 4
            && !leading_whitespace
            && !trailing_whitespace
            && contains_lines_ending_in_whitespace
        {
            return ScalarStyle::QuotedTrimIndent;
        }

        if contains_dquote && !contains_squote {
            return ScalarStyle::SingleQuoted;
        }

        ScalarStyle::Quoted
    }
}

impl<'r> Scalar<'r> {
    #[inline]
    pub fn new(value: &'r str, style: ScalarStyle) -> Self {
        Self {
            style,
            value,
            anchor: None,
        }
    }

    #[inline]
    pub fn plain(value: &'r str) -> Self {
        Self::new(value, ScalarStyle::Plain)
    }

    #[inline]
    pub fn quoted(value: &'r str) -> Self {
        Self::new(value, ScalarStyle::Quoted)
    }

    #[inline]
    pub fn infer_style(value: &'r str) -> Self {
        let style = ScalarStyle::infer(value);
        Self::new(value, style)
    }

    #[inline]
    pub fn with_anchor(self, anchor: impl Into<Option<&'r str>>) -> Self {
        Self {
            anchor: anchor.into(),
            ..self
        }
    }
}

impl<'r> AsRef<str> for Scalar<'r> {
    #[inline]
    fn as_ref(&self) -> &str {
        self.value
    }
}
