use crate::{Scalar, ScalarStyle, Span, Spanned, SpannedExt};

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
    /// without keys (sequence-like items) are preceded by an `EmptyKey` event.
    BeginMapping {
        span: Span,
        type_tag: Option<Spanned<Scalar<'r>>>,
        anchor: Option<Spanned<&'r str>>,
    },
    /// End a mapping.
    EndMapping(Span),
    /// Artificial event indicating an empty value.
    ///
    /// This may be emitted synthetically (with no relation to any particular
    /// token) by the parser in the following two circumstances:
    ///
    /// 1. A free-standing ("keyless") value in a mapping, where the mapping is
    ///    treated as having sequence-like items.
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

/// Event type (without associated data).
///
/// See [`Event`] for the full event type.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum EventType {
    Scalar(ScalarStyle),
    Ref,
    Merge,
    BeginSequence(SequenceStyle),
    EndSequence,
    BeginMapping,
    EndMapping,
    Empty,
    Comment,
}

/// An owned event.
///
/// `CachedToken` holds an `Option<Event>`, but stores the strings inside the
/// event such that allocations are amortized when calling
/// `CachedEvent::set()`.
#[derive(Clone, Debug, Default)]
pub struct CachedEvent {
    ty: Option<(EventType, Span)>,
    scalar_or_type_tag_buffer: String,
    type_tag_span: Option<Span>,
    anchor_buffer: String,
    anchor_span: Option<Span>,
}

impl CachedEvent {
    pub fn is_some(&self) -> bool {
        self.ty.is_some()
    }

    pub fn ty(&self) -> Option<EventType> {
        self.ty.map(|(ty, _)| ty)
    }

    /// Get the stored event.
    pub fn get(&self) -> Option<Event<'_>> {
        let Some((ty, span)) = self.ty else {
            return None;
        };
        let anchor = self
            .anchor_span
            .map(|anchor_span| self.anchor_buffer.as_str().in_span(anchor_span));
        let type_tag = self.type_tag_span.map(|type_tag_span| {
            Scalar {
                value: &self.scalar_or_type_tag_buffer.as_str(),
                style: ScalarStyle::Plain, // TODO: Preserve scalar style
                anchor: None,
            }
            .in_span(type_tag_span)
        });

        Some(match ty {
            EventType::Scalar(style) => Event::Scalar(
                Scalar {
                    value: &self.scalar_or_type_tag_buffer,
                    style,
                    anchor: anchor.map(Spanned::into_inner), // TODO: Preserve span
                }
                .in_span(span),
            ),
            EventType::Ref => Event::Ref(self.scalar_or_type_tag_buffer.as_str().in_span(span)),
            EventType::Merge => Event::Merge(self.scalar_or_type_tag_buffer.as_str().in_span(span)),
            EventType::BeginSequence(style) => Event::BeginSequence {
                span,
                style,
                type_tag,
                anchor,
            },
            EventType::EndSequence => Event::EndSequence(span),
            EventType::BeginMapping => Event::BeginMapping {
                span,
                type_tag,
                anchor,
            },
            EventType::EndMapping => Event::EndMapping(span),
            EventType::Empty => Event::Empty(span),
            EventType::Comment => {
                Event::Comment(self.scalar_or_type_tag_buffer.as_str().in_span(span))
            }
        })
    }

    pub fn clear(&mut self) {
        self.ty = None;
        self.scalar_or_type_tag_buffer.clear();
        self.type_tag_span = None;
        self.anchor_buffer.clear();
        self.anchor_span = None;
    }

    /// Set the stored event.
    pub fn set(&mut self, event: Option<Event>) {
        self.clear();
        let Some(event) = event else {
            return;
        };

        match event {
            Event::Scalar(Spanned {
                value:
                    Scalar {
                        value,
                        style,
                        anchor,
                    },
                span,
            }) => {
                self.ty = Some((EventType::Scalar(style), span));
                self.scalar_or_type_tag_buffer.push_str(value);
                if let Some(anchor) = anchor {
                    self.anchor_buffer.push_str(anchor);
                    self.anchor_span = Some(Span::default()); // TODO: Preserve span
                }
            }
            Event::Ref(Spanned { value, span }) => {
                self.ty = Some((EventType::Ref, span));
                self.scalar_or_type_tag_buffer.push_str(value);
            }
            Event::Merge(Spanned { value, span }) => {
                self.ty = Some((EventType::Merge, span));
                self.scalar_or_type_tag_buffer.push_str(value);
            }
            Event::BeginSequence {
                span,
                style,
                type_tag,
                anchor,
            } => {
                self.ty = Some((EventType::BeginSequence(style), span));
                if let Some(type_tag) = type_tag {
                    self.scalar_or_type_tag_buffer
                        .push_str(type_tag.value.value);
                    self.type_tag_span = Some(type_tag.span);
                }
                if let Some(anchor) = anchor {
                    self.anchor_buffer.push_str(anchor.value);
                    self.anchor_span = Some(Span::default()); // TODO: Preserve span
                }
            }
            Event::EndSequence(span) => {
                self.ty = Some((EventType::EndSequence, span));
            }
            Event::BeginMapping {
                span,
                type_tag,
                anchor,
            } => {
                self.ty = Some((EventType::BeginMapping, span));
                if let Some(type_tag) = type_tag {
                    self.scalar_or_type_tag_buffer
                        .push_str(type_tag.value.value);
                    self.type_tag_span = Some(type_tag.span);
                }
                if let Some(anchor) = anchor {
                    self.anchor_buffer.push_str(anchor.value);
                    self.anchor_span = Some(Span::default()); // TODO: Preserve span
                }
            }
            Event::EndMapping(span) => {
                self.ty = Some((EventType::EndMapping, span));
            }
            Event::Empty(span) => {
                self.ty = Some((EventType::Empty, span));
            }
            Event::Comment(comment) => {
                self.ty = Some((EventType::Comment, comment.span));
                self.scalar_or_type_tag_buffer.push_str(comment.value);
            }
        }
    }
}

impl From<Event<'_>> for CachedEvent {
    #[inline]
    fn from(event: Event<'_>) -> Self {
        let mut cached = CachedEvent::default();
        cached.set(Some(event));
        cached
    }
}

impl PartialEq<Event<'_>> for CachedEvent {
    #[inline]
    fn eq(&self, other: &Event<'_>) -> bool {
        self.get() == Some(*other)
    }
}

impl<'r> Event<'r> {
    #[inline]
    pub fn to_owned(self) -> CachedEvent {
        CachedEvent::from(self)
    }
}

impl<'r> From<&'r CachedEvent> for Option<Event<'r>> {
    #[inline]
    fn from(event: &'r CachedEvent) -> Self {
        event.get()
    }
}
