use crate::{Scalar, SequenceStyle, Span, Spanned, SpannedExt};
use std::hash::Hash;

/// Any self-contained value.
///
/// Note that when comparing and hashing values, styles and tags are ignored.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Value<'r> {
    Scalar(Scalar<'r>),
    Sequence(Sequence<'r>),
}

impl<'r> Value<'r> {
    #[inline]
    pub fn plain(value: &'r str) -> Self {
        Value::Scalar(Scalar::plain(value))
    }

    #[inline]
    pub fn quoted(value: &'r str) -> Self {
        Value::Scalar(Scalar::quoted(value))
    }

    pub fn seq(items: impl IntoIterator<Item = Value<'r>>) -> Self {
        Value::Sequence(Sequence {
            anchor: None,
            type_tag: None,
            style: SequenceStyle::List,
            items: items.into_iter().map(|v| (None, v)).collect(),
        })
    }

    pub fn mapping(items: impl IntoIterator<Item = (Option<Value<'r>>, Value<'r>)>) -> Self {
        Value::Sequence(Sequence {
            anchor: None,
            type_tag: None,
            style: SequenceStyle::Mapping,
            items: items.into_iter().collect(),
        })
    }

    #[inline]
    pub fn anchor(&self) -> Option<&'r str> {
        match self {
            Value::Scalar(Scalar { anchor, .. }) => *anchor,
            Value::Sequence(seq) => seq.anchor,
        }
    }

    pub fn with_anchor(self, anchor: impl Into<Option<&'r str>>) -> Self {
        match self {
            Value::Scalar(scalar) => Value::Scalar(scalar.with_anchor(anchor)),
            Value::Sequence(seq) => Value::Sequence(Sequence {
                anchor: anchor.into(),
                ..seq
            }),
        }
    }

    pub fn to_spanned(&self) -> SpannedValue<'r> {
        match self {
            Value::Scalar(scalar) => SpannedValue::Scalar(Spanned {
                value: *scalar,
                span: Span::default(),
            }),
            Value::Sequence(seq) => SpannedValue::Sequence(SpannedSequence {
                span: Span::default(),
                anchor: seq.anchor.map(|a| Spanned {
                    value: a,
                    span: Span::default(),
                }),
                type_tag: seq.type_tag.map(|t| Spanned {
                    value: t,
                    span: Span::default(),
                }),
                style: seq.style,
                items: seq
                    .items
                    .iter()
                    .map(|(key, value)| (key.as_ref().map(|v| v.to_spanned()), value.to_spanned()))
                    .collect(),
            }),
        }
    }
}

#[derive(Debug, Clone, Eq)]
pub struct Sequence<'r> {
    pub anchor: Option<&'r str>,
    pub type_tag: Option<Scalar<'r>>,
    pub style: SequenceStyle,
    pub items: Vec<(Option<Value<'r>>, Value<'r>)>,
}

impl<'r> Hash for Sequence<'r> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.type_tag.hash(state);
        self.items.hash(state);
    }
}

impl<'r> PartialEq for Sequence<'r> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.items == other.items
    }
}

/// Same as [`Value`], but with [`Span`]s attached to each variant and each
/// value inside.
#[derive(Debug, Clone)]
pub enum SpannedValue<'r> {
    Scalar(Spanned<Scalar<'r>>),
    Sequence(SpannedSequence<'r>),
}

impl<'r> SpannedValue<'r> {
    #[inline]
    pub fn span(&self) -> Span {
        match self {
            Self::Scalar(Spanned { span, .. }) => *span,
            Self::Sequence(seq) => seq.span,
        }
    }

    #[inline]
    pub fn anchor(&self) -> Option<&'r str> {
        match self {
            Self::Scalar(Spanned { value, .. }) => value.anchor,
            Self::Sequence(seq) => seq.anchor.map(Spanned::into_inner),
        }
    }

    #[inline]
    pub fn with_anchor(self, anchor: impl Into<Option<&'r str>>) -> Self {
        match self {
            Self::Scalar(scalar) => Self::Scalar(scalar.map(|s| s.with_anchor(anchor))),
            Self::Sequence(seq) => Self::Sequence(SpannedSequence {
                anchor: anchor.into().map(|anchor| anchor.in_span(Span::default())),
                ..seq
            }),
        }
    }
}

impl<'a, 'b> PartialEq<SpannedValue<'b>> for SpannedValue<'a> {
    #[inline]
    fn eq(&self, other: &SpannedValue<'b>) -> bool {
        match (self, other) {
            (SpannedValue::Scalar(a), SpannedValue::Scalar(b)) => a.value == b.value,
            (SpannedValue::Sequence(a), SpannedValue::Sequence(b)) => a == b,
            _ => false,
        }
    }
}

impl<'a, 'b> PartialEq<Value<'b>> for SpannedValue<'a> {
    #[inline]
    fn eq(&self, other: &Value<'b>) -> bool {
        match (self, other) {
            (SpannedValue::Scalar(a), Value::Scalar(b)) => a.value == *b,
            (SpannedValue::Sequence(a), Value::Sequence(b)) => a == b,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SpannedSequence<'r> {
    pub span: Span,
    pub anchor: Option<Spanned<&'r str>>,
    pub type_tag: Option<Spanned<Scalar<'r>>>,
    pub style: SequenceStyle,
    pub items: Vec<(Option<SpannedValue<'r>>, SpannedValue<'r>)>,
}

impl<'r> SpannedSequence<'r> {
    #[inline]
    pub fn type_tag(&self) -> Option<&'r str> {
        self.type_tag.as_ref().map(|tag| tag.value.value)
    }
}

impl<'a, 'b> PartialEq<SpannedSequence<'b>> for SpannedSequence<'a> {
    #[inline]
    fn eq(&self, other: &SpannedSequence<'b>) -> bool {
        self.style == other.style && self.items == other.items
    }
}

impl<'a, 'b> PartialEq<Sequence<'b>> for SpannedSequence<'a> {
    #[inline]
    fn eq(&self, other: &Sequence<'b>) -> bool {
        let mut lhs = self.items.iter();
        let mut rhs = other.items.iter();
        while let Some((a_key, a_value)) = lhs.next() {
            if let Some((b_key, b_value)) = rhs.next() {
                match (a_key, b_key) {
                    (Some(a_key), Some(b_key)) => {
                        if a_key != b_key {
                            return false;
                        }
                    }
                    (None, None) => {}
                    _ => return false,
                }

                if a_value != b_value {
                    return false;
                }
            } else {
                return false;
            }
        }

        // Check that the iterators have the same length.
        rhs.next().is_none()
    }
}

impl<'r> From<SpannedSequence<'r>> for Sequence<'r> {
    #[inline]
    fn from(seq: SpannedSequence<'r>) -> Self {
        Sequence {
            anchor: seq.anchor.map(Spanned::into_inner),
            type_tag: seq.type_tag.map(Spanned::into_inner),
            style: seq.style,
            items: seq
                .items
                .into_iter()
                .map(|(key, value)| (key.map(Into::into), value.into()))
                .collect(),
        }
    }
}

impl<'r> From<SpannedValue<'r>> for Value<'r> {
    #[inline]
    fn from(value: SpannedValue<'r>) -> Self {
        match value {
            SpannedValue::Scalar(Spanned { value, .. }) => Value::Scalar(value),
            SpannedValue::Sequence(seq) => Value::Sequence(seq.into()),
        }
    }
}
