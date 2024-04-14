#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Comment<'r> {
    /// The comment text, excluding the `#` prefix, but including any spaces
    /// after `#`. The message may contain newlines if more than one `#` line is
    /// present without newlines between them.
    pub message: &'r str,
    /// The structural placement of the comment, used by formatters.
    pub adhere: CommentAdhesion,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CommentAdhesion {
    /// Free-floating comment within a parent container. Its general placement
    /// should be preserved in the flow of items, but it should be spaced away
    /// from particular items.
    ///
    /// The `#` on each line is only preceded by whitespace.
    Flow,
    /// The comment is at the end of a line containing other tokens, and should
    /// adhere to the preceding tokens.
    AdhereBefore,
    /// The comment is on a line immediately preceding other (non-delimiter)
    /// tokens, and should adhere to those tokens.
    AdhereAfter,
}
