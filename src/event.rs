use crate::{OwnedScalar, Scalar, ScalarStyle, Span, Spanned, SpannedExt};

#[derive(Clone, Copy, Debug)]
pub enum Event<'r> {
    /// A scalar value.
    Scalar(Spanned<Scalar<'r>>),
    /// A reference to a value that has previously been assigned a tag.
    Ref(Spanned<&'r str>),
    /// Merge a tagged sequence into the current sequence.
    Merge(Spanned<&'r str>),
    /// Begin a sequence.
    BeginSequence {
        span: Span,
        style: SequenceStyle,
        type_tag: Option<Spanned<Scalar<'r>>>,
        anchor: Option<Spanned<&'r str>>,
    },
    /// End a sequence.
    EndSequence(Span),
    /// Begin a mapping. Key/value pairs are emitted for each entry. Values
    /// without keys (sequence-like entries) are preceded by an `EmptyKey`
    /// event.
    BeginMapping {
        span: Span,
        type_tag: Option<Spanned<Scalar<'r>>>,
        anchor: Option<Spanned<&'r str>>,
    },
    /// End a mapping.
    EndMapping(Span),
    /// Artificial event indicating an empty value.
    ///
    /// This is emitted artificially (with no relation to any particular token)
    /// by the parser in the following two circumstances:
    ///
    /// 1. A free-standing ("keyless") value in a mapping, where the mapping is
    ///    treated as having sequence-like elements.
    /// 2. An empty value in a mapping, i.e. `:` followed by a newline. Note
    ///    that a trailing comma is not allowed in this case.
    Empty(Span),
    /// Comment
    Comment(Spanned<&'r str>),
}

impl<'r> Event<'r> {
    #[inline]
    pub fn empty() -> Self {
        Self::Empty(Span::default())
    }

    #[inline]
    pub fn scalar(scalar: Scalar<'r>) -> Self {
        Self::Scalar(scalar.in_span(Span::default()))
    }

    #[inline]
    pub fn plain(value: &'r str) -> Self {
        Self::scalar(Scalar::new(value, ScalarStyle::Plain))
    }

    #[inline]
    pub fn quoted(value: &'r str) -> Self {
        Self::scalar(Scalar::new(value, ScalarStyle::Quoted))
    }

    #[inline]
    pub fn reference(anchor: &'r str) -> Self {
        Self::Ref(anchor.in_span(Span::default()))
    }

    #[inline]
    pub fn merge(anchor: &'r str) -> Self {
        Self::Merge(anchor.in_span(Span::default()))
    }

    #[inline]
    pub fn comment(comment: &'r str) -> Self {
        Self::Comment(comment.in_span(Span::default()))
    }

    #[inline]
    pub fn begin_mapping(type_tag: Option<&'r str>, anchor: Option<&'r str>) -> Self {
        Self::BeginMapping {
            span: Span::default(),
            type_tag: type_tag.map(|t| Scalar::plain(t).in_span(Span::default())),
            anchor: anchor.map(|a| a.in_span(Span::default())),
        }
    }

    #[inline]
    pub fn end_mapping() -> Self {
        Self::EndMapping(Span::default())
    }

    #[inline]
    pub fn begin_sequence(
        style: SequenceStyle,
        type_tag: Option<&'r str>,
        anchor: Option<&'r str>,
    ) -> Self {
        Self::BeginSequence {
            span: Span::default(),
            style,
            type_tag: type_tag.map(|t| Scalar::plain(t).in_span(Span::default())),
            anchor: anchor.map(|a| a.in_span(Span::default())),
        }
    }

    #[inline]
    pub fn end_sequence() -> Self {
        Self::EndSequence(Span::default())
    }
}

impl<'a, 'b> PartialEq<Event<'b>> for Event<'a> {
    #[inline]
    fn eq(&self, other: &Event<'b>) -> bool {
        match (self, other) {
            (Event::Scalar(a), Event::Scalar(b)) => **a == **b,
            (Event::Ref(a), Event::Ref(b)) => **a == **b,
            (Event::Merge(a), Event::Merge(b)) => **a == **b,
            (
                Event::BeginSequence {
                    style: a,
                    type_tag: b,
                    anchor: c,
                    ..
                },
                Event::BeginSequence {
                    style: d,
                    type_tag: e,
                    anchor: f,
                    ..
                },
            ) => a == d && *b == *e && *c == *f,
            (Event::EndSequence(_), Event::EndSequence(_)) => true,
            (
                Event::BeginMapping {
                    type_tag: a,
                    anchor: b,
                    ..
                },
                Event::BeginMapping {
                    type_tag: c,
                    anchor: d,
                    ..
                },
            ) => a == c && b == d,
            (Event::EndMapping(_), Event::EndMapping(_)) => true,
            (Event::Empty(_), Event::Empty(_)) => true,
            (Event::Comment(a), Event::Comment(b)) => a == b,
            _ => false,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SequenceStyle {
    /// A sequence that is represented by curly braces, and may contain key-value pairs.
    Mapping,
    /// A sequence that is represented by square brackets, which cannot contain key-value pairs.
    List,
    /// A sequence that is represented by parentheses, which cannot contain key-value pairs.
    Tuple,
}

#[derive(Clone, Debug, Hash)]
pub enum OwnedEvent {
    /// A scalar value.
    Scalar(Spanned<OwnedScalar>),
    /// A reference to a value that has previously been assigned an anchor.
    Ref(Spanned<String>),
    /// Merge a sequence with an anchor into the current sequence.
    Merge(Spanned<String>),
    /// Begin a sequence.
    BeginSequence {
        span: Span,
        style: SequenceStyle,
        type_tag: Option<Spanned<OwnedScalar>>,
        anchor: Option<Spanned<String>>,
    },
    /// End a sequence.
    EndSequence(Span),
    /// Begin a mapping. Key/value pairs are emitted for each entry. Values
    /// without keys (sequence-like entries) are preceded by an `EmptyKey`
    /// event.
    BeginMapping {
        span: Span,
        type_tag: Option<Spanned<OwnedScalar>>,
        anchor: Option<Spanned<String>>,
    },
    /// End a mapping.
    EndMapping(Span),
    /// Artificial event indicating an empty value.
    ///
    /// This is emitted artificially (with no relation to any particular token)
    /// by the parser in the following two circumstances:
    ///
    /// 1. A free-standing ("keyless") value in a mapping, where the mapping is
    ///    treated as having sequence-like elements.
    /// 2. An empty value in a mapping, i.e. `:` followed by a newline. Note
    ///    that a trailing comma is not allowed in this case.
    Empty(Span),
    /// Comment
    Comment(Spanned<String>),
}

impl PartialEq<Event<'_>> for OwnedEvent {
    #[inline]
    fn eq(&self, other: &Event<'_>) -> bool {
        self.borrow() == *other
    }
}

impl OwnedEvent {
    pub fn borrow(&self) -> Event {
        match self {
            OwnedEvent::Scalar(scalar) => Event::Scalar(scalar.as_ref().map(OwnedScalar::borrow)),
            OwnedEvent::Ref(r) => Event::Ref(r.as_ref().map(String::as_str)),
            OwnedEvent::Merge(m) => Event::Merge(m.as_ref().map(String::as_str)),
            OwnedEvent::BeginSequence {
                span,
                style,
                type_tag,
                anchor,
            } => Event::BeginSequence {
                span: *span,
                style: *style,
                type_tag: type_tag
                    .as_ref()
                    .map(|t| t.as_ref().map(OwnedScalar::borrow)),
                anchor: anchor.as_ref().map(|a| a.as_ref().map(String::as_ref)),
            },
            OwnedEvent::EndSequence(span) => Event::EndSequence(*span),
            OwnedEvent::BeginMapping {
                span,
                type_tag,
                anchor,
            } => Event::BeginMapping {
                span: *span,
                type_tag: type_tag
                    .as_ref()
                    .map(|t| t.as_ref().map(OwnedScalar::borrow)),
                anchor: anchor.as_ref().map(|a| a.as_ref().map(String::as_ref)),
            },
            OwnedEvent::EndMapping(span) => Event::EndMapping(*span),
            OwnedEvent::Empty(span) => Event::Empty(*span),
            OwnedEvent::Comment(c) => Event::Comment(c.as_ref().map(String::as_str)),
        }
    }
}

impl<'r> Event<'r> {
    #[inline]
    pub fn to_owned(self) -> OwnedEvent {
        match self {
            Event::Scalar(scalar) => OwnedEvent::Scalar(scalar.map(Scalar::to_owned)),
            Event::Ref(r) => OwnedEvent::Ref(r.map(ToOwned::to_owned)),
            Event::Merge(m) => OwnedEvent::Merge(m.map(ToOwned::to_owned)),
            Event::BeginSequence {
                span,
                style,
                type_tag,
                anchor,
            } => OwnedEvent::BeginSequence {
                span,
                style,
                type_tag: type_tag.map(|t| t.map(Scalar::to_owned)),
                anchor: anchor.map(|a| a.map(ToOwned::to_owned)),
            },
            Event::EndSequence(span) => OwnedEvent::EndSequence(span),
            Event::BeginMapping {
                span,
                type_tag,
                anchor,
            } => OwnedEvent::BeginMapping {
                span,
                type_tag: type_tag.map(|t| t.map(Scalar::to_owned)),
                anchor: anchor.map(|a| a.map(ToOwned::to_owned)),
            },
            Event::EndMapping(span) => OwnedEvent::EndMapping(span),
            Event::Empty(span) => OwnedEvent::Empty(span),
            Event::Comment(c) => OwnedEvent::Comment(c.map(ToOwned::to_owned)),
        }
    }
}

impl<'r> From<&'r OwnedEvent> for Event<'r> {
    #[inline]
    fn from(event: &'r OwnedEvent) -> Self {
        event.borrow()
    }
}
