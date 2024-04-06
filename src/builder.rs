use std::{
    collections::{hash_map, HashSet},
    io::BufRead,
    num::NonZeroUsize,
};

use crate::{
    Document, Error, Node, NodeData, NodeId, OwnedEvent, OwnedScalar, ParseStream, ScalarStyle,
    ScannerError, SequenceItem, SequenceNode, SequenceStyle, Span, Spanned, SpannedExt,
    SpannedValue, Value,
};

#[derive(Debug, thiserror::Error)]
pub enum BuilderError {
    /// An anchor is referenced before it is defined.
    #[error("undefined tag anchor: {0}")]
    UndefinedAnchor(String),
    /// An known anchor is referenced, but its definition is not yet complete
    /// (recursive reference).
    #[error("cannot reference an incomplete sequence (circular references are not allowed): {0}")]
    IncompleteAnchor(String),
    /// The same anchor is used multiple times.
    #[error("duplicate anchor: {0}")]
    DuplicateAnchor(String),
    /// The same node has multiple anchors.
    #[error("multiple anchors for the same node: {0}")]
    MultipleAnchors(String, String),
    #[error("unexpected event while building document")]
    UnexpectedEvent,
    #[error("merge into node that is not a sequence")]
    MergeIntoNonSequence,
}

/// Document builder API.
#[derive(Default)]
pub struct Builder {
    doc: Document,
    incomplete_sequences: HashSet<NodeId>,
    incomplete_anchors: HashSet<String>,
}

impl Builder {
    fn set_anchor(&mut self, node_id: NodeId, tag: Option<String>) -> Result<(), BuilderError> {
        let Some(tag) = tag else { return Ok(()) };

        match self.doc.anchors.entry(tag.clone()) {
            hash_map::Entry::Occupied(entry) => {
                return Err(BuilderError::DuplicateAnchor(entry.key().clone()));
            }
            hash_map::Entry::Vacant(entry) => {
                if self.incomplete_anchors.contains(entry.key()) {
                    return Err(BuilderError::DuplicateAnchor(entry.key().clone()));
                }
                entry.insert(node_id);
                Ok(())
            }
        }
    }

    fn add_spanned_scalar(&mut self, value: Spanned<OwnedScalar>) -> Result<NodeId, BuilderError> {
        let node_id = NodeId(NonZeroUsize::new(self.doc.nodes.len() + 1).unwrap());

        self.set_anchor(node_id, value.anchor.clone())?;

        self.doc.nodes.push(Node {
            anchor: value
                .anchor
                .clone()
                .map(|anchor| anchor.in_span(Span::default())),
            data: NodeData::Scalar(value.value),
            span: value.span,
        });

        Ok(node_id)
    }

    #[inline]
    pub(crate) fn add_scalar(&mut self, value: OwnedScalar) -> Result<NodeId, BuilderError> {
        self.add_spanned_scalar(value.in_span(Span::default()))
    }

    #[inline]
    pub(crate) fn add_scalar_infer_style(
        &mut self,
        value: &str,
        anchor: Option<&str>,
    ) -> Result<NodeId, BuilderError> {
        self.add_spanned_scalar(
            OwnedScalar::infer_style(value)
                .with_anchor(anchor.map(ToOwned::to_owned))
                .in_span(Span::default()),
        )
    }

    fn add_spanned_sequence(
        &mut self,
        style: SequenceStyle,
        anchor: Option<Spanned<String>>,
        type_tag: Option<NodeId>,
        span: Span,
    ) -> Result<NodeId, BuilderError> {
        let node_id = NodeId(NonZeroUsize::new(self.doc.nodes.len() + 1).unwrap());

        if let Some(tag) = anchor.clone().map(Spanned::into_inner) {
            self.set_anchor(node_id, Some(tag.clone()))?;
            self.incomplete_anchors.insert(tag);
        }
        self.incomplete_sequences.insert(node_id);

        self.doc.nodes.push(Node {
            anchor,
            data: NodeData::Sequence(SequenceNode {
                style,
                type_tag,
                items: vec![],
            }),
            span,
        });

        Ok(node_id)
    }

    pub(crate) fn complete_sequence(&mut self, node: NodeId) {
        self.incomplete_sequences.remove(&node);
        if let Some(tag) = self.doc.nodes[node].anchor.as_ref() {
            let removed = self.incomplete_anchors.remove(tag.value.as_str());
            assert!(removed, "tag was not marked as incomplete");
        }
    }

    pub(crate) fn add_sequence(
        &mut self,
        style: SequenceStyle,
        anchor: Option<String>,
        type_tag: Option<NodeId>,
    ) -> Result<NodeId, BuilderError> {
        self.add_spanned_sequence(
            style,
            anchor.map(|anchor| anchor.in_span(Span::default())),
            type_tag,
            Span::default(),
        )
    }

    pub(crate) fn add_sequence_item(
        &mut self,
        seq: NodeId,
        key_node: Option<NodeId>,
        value_node: NodeId,
    ) {
        if let NodeData::Sequence(ref mut seq) = self.doc.nodes[seq].data {
            seq.items.push(SequenceItem {
                key_node,
                value_node,
            });
        } else {
            panic!("not a sequence node")
        }
    }

    pub(crate) fn add_root_item(&mut self, key_node: Option<NodeId>, value_node: NodeId) {
        self.doc.sequence.items.push(SequenceItem {
            key_node,
            value_node,
        });
    }

    fn add_spanned_value(&mut self, value: SpannedValue) -> Result<NodeId, BuilderError> {
        match value {
            SpannedValue::Scalar(scalar) => {
                self.add_spanned_scalar(scalar.map(|value| value.to_owned()))
            }
            SpannedValue::Sequence(seq) => {
                let type_tag = seq
                    .type_tag
                    .map(|tag| self.add_scalar(tag.into_inner().into()))
                    .transpose()?;
                let node = self.add_spanned_sequence(
                    seq.style,
                    seq.anchor.map(|s| s.map(ToOwned::to_owned)),
                    type_tag,
                    seq.span,
                )?;
                let mut builder = MappingBuilder {
                    node: Some(node),
                    builder: self,
                };
                for (key, value) in seq.items {
                    if let Some(key) = key {
                        builder.entry(key, value)?;
                    } else {
                        builder.item(value)?;
                    }
                }
                Ok(node)
            }
        }
    }

    fn add_value(&mut self, value: Value) -> Result<NodeId, BuilderError> {
        match value {
            Value::Scalar(ref scalar) => self.add_scalar((*scalar).to_owned()),
            Value::Sequence(seq) => {
                let type_tag = seq
                    .type_tag
                    .map(|tag| self.add_scalar(tag.to_owned()))
                    .transpose()?;
                let node =
                    self.add_sequence(seq.style, seq.anchor.map(ToOwned::to_owned), type_tag)?;

                let mut builder = MappingBuilder {
                    node: Some(node),
                    builder: self,
                };
                for (key, value) in seq.items {
                    if let Some(key) = key {
                        builder.entry(key, value)?;
                    } else {
                        builder.item(value)?;
                    }
                }
                Ok(node)
            }
        }
    }

    fn resolve_tag(&self, anchor: &str) -> Result<NodeId, BuilderError> {
        if self.incomplete_anchors.contains(anchor) {
            return Err(BuilderError::IncompleteAnchor(anchor.to_owned()));
        }
        if let Some(node) = self.doc.anchors.get(anchor) {
            Ok(*node)
        } else {
            Err(BuilderError::UndefinedAnchor(anchor.to_owned()))
        }
    }

    pub fn build(&mut self) -> Result<Document, BuilderError> {
        if self.incomplete_sequences.is_empty() {
            assert!(self.incomplete_anchors.is_empty());
            Ok(std::mem::take(&mut self.doc))
        } else {
            panic!("Incomplete sequences in builder; perhaps you somehow called `forget(...)` on a sequence builder?")
        }
    }

    fn merge(&mut self, target: Option<NodeId>, anchor: &str) -> Result<(), BuilderError> {
        let source = self.resolve_tag(anchor)?;
        assert_ne!(target, Some(source), "cannot merge a node with itself");

        let (src, dst) = if let Some(target) = target {
            // TODO: Replace this with slice::get_many_mut when stable
            let src_idx = source.0.get() - 1;
            let dst_idx = target.0.get() - 1;
            let (src, dst_node) = if src_idx < dst_idx {
                let (lower, upper) = self.doc.nodes.split_at_mut(dst_idx);
                (&mut lower[src_idx], &mut upper[0])
            } else {
                let (lower, upper) = self.doc.nodes.split_at_mut(src_idx);
                (&mut upper[0], &mut lower[dst_idx])
            };
            if let NodeData::Sequence(dst_seq) = &mut dst_node.data {
                (src, dst_seq)
            } else {
                panic!("cannot merge into non-sequence")
            }
        } else {
            (&mut self.doc.nodes[source], &mut self.doc.sequence)
        };

        if let NodeData::Sequence(src_seq) = &src.data {
            for item in src_seq.items.iter() {
                dst.items.push(item.clone());
            }
        } else {
            dst.items.push(SequenceItem {
                key_node: None,
                value_node: source,
            });
        }

        Ok(())
    }

    pub fn entry(
        &mut self,
        key: impl BuildValue,
        value: impl BuildValue,
    ) -> Result<&mut Self, BuilderError> {
        let key_node = key.build(None, None, self)?;
        let value_node = value.build(None, None, self)?;
        self.doc.sequence.items.push(SequenceItem {
            key_node: Some(key_node),
            value_node,
        });
        Ok(self)
    }

    pub fn item(&mut self, value: impl BuildValue) -> Result<&mut Self, BuilderError> {
        let value_node = value.build(None, None, self)?;
        self.doc.sequence.items.push(SequenceItem {
            key_node: None,
            value_node,
        });
        Ok(self)
    }
}

pub struct SequenceBuilder<'b> {
    inner: MappingBuilder<'b>,
}

pub struct MappingBuilder<'b> {
    pub(crate) node: Option<NodeId>,
    pub(crate) builder: &'b mut Builder,
}

impl<'b> MappingBuilder<'b> {
    pub(crate) fn parse_root<R: BufRead>(
        &mut self,
        parser: &mut ParseStream<R>,
    ) -> Result<(), Error> {
        self.parse(parser, true)
    }

    fn parse<R: BufRead>(
        &mut self,
        parser: &mut ParseStream<R>,
        is_root: bool,
    ) -> Result<(), Error> {
        let mut pending_key: Option<NodeId> = None;

        loop {
            let Some(event) = parser.next_event()? else {
                if !is_root {
                    return Err(ScannerError::UnexpectedEof.into());
                } else {
                    return Ok(());
                }
            };
            let event = event.to_owned();

            let key_or_value = match event {
                OwnedEvent::Scalar(scalar) => self.builder.add_spanned_scalar(scalar)?,
                OwnedEvent::Ref(anchor) => self.builder.resolve_tag(&anchor)?,
                OwnedEvent::Merge(anchor) => {
                    if pending_key.is_some() {
                        return Err(BuilderError::UnexpectedEvent.into());
                    }
                    self.builder.merge(self.node, &anchor)?;
                    continue;
                }
                OwnedEvent::BeginSequence {
                    span,
                    style,
                    type_tag,
                    anchor,
                } => {
                    let type_tag = type_tag
                        .map(|type_tag| self.builder.add_spanned_scalar(type_tag))
                        .transpose()?;

                    let seq_id = self
                        .builder
                        .add_spanned_sequence(style, anchor, type_tag, span)?;
                    let mut seq_builder = SequenceBuilder {
                        inner: MappingBuilder {
                            node: Some(seq_id),
                            builder: self.builder,
                        },
                    };
                    seq_builder.parse(parser)?;
                    seq_id
                }
                OwnedEvent::EndSequence(_) => return Err(BuilderError::UnexpectedEvent.into()),
                OwnedEvent::BeginMapping {
                    span,
                    type_tag,
                    anchor,
                } => {
                    let type_tag = type_tag
                        .map(|type_tag| self.builder.add_spanned_scalar(type_tag))
                        .transpose()?;

                    let submap_id = self.builder.add_spanned_sequence(
                        SequenceStyle::Mapping,
                        anchor,
                        type_tag,
                        span,
                    )?;
                    let mut submap_builder = MappingBuilder {
                        node: Some(submap_id),
                        builder: self.builder,
                    };
                    submap_builder.parse(parser, false)?;
                    submap_id
                }
                OwnedEvent::EndMapping(_) => {
                    if pending_key.is_some() {
                        return Err(BuilderError::UnexpectedEvent.into());
                    }
                    return Ok(());
                }
                OwnedEvent::Empty(span) => self.builder.add_spanned_scalar(Spanned {
                    value: OwnedScalar {
                        style: ScalarStyle::Plain,
                        value: String::new(),
                        anchor: None,
                    },
                    span,
                })?,
                OwnedEvent::Comment(_) => continue,
            };

            if let Some(key) = pending_key.take() {
                self.seq().items.push(SequenceItem {
                    key_node: Some(key),
                    value_node: key_or_value,
                });
            } else {
                pending_key = Some(key_or_value);
            }
        }
    }

    fn seq(&mut self) -> &mut SequenceNode {
        if let Some(node) = self.node {
            if let NodeData::Sequence(ref mut seq) = self.builder.doc.nodes[node].data {
                seq
            } else {
                unreachable!("mapping is not a sequence node")
            }
        } else {
            &mut self.builder.doc.sequence
        }
    }

    pub fn entry(
        &mut self,
        key: impl BuildValue,
        value: impl BuildValue,
    ) -> Result<&mut Self, BuilderError> {
        let key_node = key.build(None, None, self.builder)?;
        let value_node = value.build(None, None, self.builder)?;
        self.seq().items.push(SequenceItem {
            key_node: Some(key_node),
            value_node,
        });
        Ok(self)
    }

    pub fn item(&mut self, value: impl BuildValue) -> Result<&mut Self, BuilderError> {
        let value_node = value.build(None, None, self.builder)?;
        self.seq().items.push(SequenceItem {
            key_node: None,
            value_node,
        });
        Ok(self)
    }
}

impl<'b> Drop for MappingBuilder<'b> {
    #[inline]
    fn drop(&mut self) {
        if let Some(node) = self.node {
            self.builder.complete_sequence(node);
        }
    }
}

impl<'b> SequenceBuilder<'b> {
    fn parse<R: BufRead>(&mut self, parser: &mut ParseStream<R>) -> Result<(), Error> {
        loop {
            let Some(event) = parser.next_event()? else {
                return Err(ScannerError::UnexpectedEof.into());
            };
            let event = event.to_owned();

            let value = match event {
                OwnedEvent::Scalar(scalar) => self.inner.builder.add_spanned_scalar(scalar)?,
                OwnedEvent::Ref(anchor) => self.inner.builder.resolve_tag(&anchor)?,
                OwnedEvent::Merge(anchor) => {
                    self.inner.builder.merge(self.inner.node, &anchor)?;
                    continue;
                }
                OwnedEvent::BeginSequence {
                    span,
                    style,
                    type_tag,
                    anchor,
                } => {
                    let type_tag = type_tag
                        .map(|type_tag| self.inner.builder.add_spanned_scalar(type_tag))
                        .transpose()?;

                    let seq_id = self
                        .inner
                        .builder
                        .add_spanned_sequence(style, anchor, type_tag, span)?;
                    let mut seq_builder = SequenceBuilder {
                        inner: MappingBuilder {
                            node: Some(seq_id),
                            builder: self.inner.builder,
                        },
                    };
                    seq_builder.parse(parser)?;
                    seq_id
                }
                OwnedEvent::EndSequence(span) => {
                    // TODO: Merge spans
                    return Ok(());
                }
                OwnedEvent::BeginMapping {
                    span,
                    type_tag,
                    anchor,
                } => {
                    let type_tag = type_tag
                        .map(|type_tag| self.inner.builder.add_spanned_scalar(type_tag))
                        .transpose()?;

                    let submap_id = self.inner.builder.add_spanned_sequence(
                        SequenceStyle::Mapping,
                        anchor,
                        type_tag,
                        span,
                    )?;
                    let mut submap_builder = MappingBuilder {
                        node: Some(submap_id),
                        builder: self.inner.builder,
                    };
                    submap_builder.parse(parser, false)?;
                    submap_id
                }
                OwnedEvent::EndMapping(_) => return Err(BuilderError::UnexpectedEvent.into()),
                OwnedEvent::Empty(span) => self.inner.builder.add_spanned_scalar(Spanned {
                    value: OwnedScalar {
                        style: ScalarStyle::Plain,
                        value: String::new(),
                        anchor: None,
                    },
                    span,
                })?,
                OwnedEvent::Comment(_) => continue,
            };

            self.inner.seq().items.push(SequenceItem {
                key_node: None,
                value_node: value,
            });
        }
    }

    pub fn item(&mut self, value: impl BuildValue) -> Result<&mut Self, BuilderError> {
        let value_node = value.build(None, None, self.inner.builder)?;
        self.inner.seq().items.push(SequenceItem {
            key_node: None,
            value_node,
        });
        Ok(self)
    }
}

pub trait BuildValue {
    #[doc(hidden)]
    fn build(
        self,
        anchor: Option<&str>,
        type_tag: Option<&str>,
        builder: &mut Builder,
    ) -> Result<NodeId, BuilderError>;

    fn with_anchor(self, anchor: &str) -> BuildWithAnchor<Self>
    where
        Self: Sized,
    {
        BuildWithAnchor {
            inner: self,
            anchor,
        }
    }
}

pub trait BuildSeq: BuildValue {
    fn with_type_tag(self, type_tag: &str) -> BuildWithTypeTag<Self>
    where
        Self: Sized,
    {
        BuildWithTypeTag {
            inner: self,
            type_tag,
        }
    }
}

pub struct BuildWithAnchor<'a, T> {
    inner: T,
    anchor: &'a str,
}

impl<'a, T: BuildValue> BuildValue for BuildWithAnchor<'a, T> {
    #[inline]
    fn build(
        self,
        anchor: Option<&str>,
        type_tag: Option<&str>,
        builder: &mut Builder,
    ) -> Result<NodeId, BuilderError> {
        if let Some(anchor) = anchor {
            return Err(BuilderError::MultipleAnchors(
                self.anchor.to_owned(),
                anchor.to_owned(),
            ));
        }
        self.inner.build(Some(self.anchor), type_tag, builder)
    }
}

pub struct BuildWithTypeTag<'a, T> {
    inner: T,
    type_tag: &'a str,
}

impl<'a, T: BuildSeq> BuildValue for BuildWithTypeTag<'a, T> {
    #[inline]
    fn build(
        self,
        anchor: Option<&str>,
        type_tag: Option<&str>,
        builder: &mut Builder,
    ) -> Result<NodeId, BuilderError> {
        assert!(type_tag.is_none());
        self.inner.build(anchor, Some(self.type_tag), builder)
    }
}

impl<'a> BuildValue for &'a str {
    #[inline]
    fn build(
        self,
        anchor: Option<&str>,
        type_tag: Option<&str>,
        builder: &mut Builder,
    ) -> Result<NodeId, BuilderError> {
        assert!(type_tag.is_none());
        builder.add_scalar_infer_style(self, anchor)
    }
}

impl BuildValue for String {
    #[inline]
    fn build(
        self,
        anchor: Option<&str>,
        type_tag: Option<&str>,
        builder: &mut Builder,
    ) -> Result<NodeId, BuilderError> {
        assert!(type_tag.is_none());
        builder.add_scalar_infer_style(&self, anchor)
    }
}

impl BuildValue for char {
    #[inline]
    fn build(
        self,
        anchor: Option<&str>,
        type_tag: Option<&str>,
        builder: &mut Builder,
    ) -> Result<NodeId, BuilderError> {
        assert!(type_tag.is_none());
        builder.add_scalar_infer_style(&self.to_string(), anchor)
    }
}

impl<'a> BuildValue for Value<'a> {
    #[inline]
    fn build(
        self,
        anchor: Option<&str>,
        type_tag: Option<&str>,
        builder: &mut Builder,
    ) -> Result<NodeId, BuilderError> {
        assert!(type_tag.is_none());
        if let Some(anchor) = anchor {
            if let Some(previous_anchor) = self.anchor() {
                return Err(BuilderError::MultipleAnchors(
                    anchor.to_owned(),
                    previous_anchor.to_owned(),
                ));
            }
            builder.add_value(self.with_anchor(anchor))
        } else {
            builder.add_value(self)
        }
    }
}

impl<'a> BuildValue for SpannedValue<'a> {
    #[inline]
    fn build(
        self,
        anchor: Option<&str>,
        type_tag: Option<&str>,
        builder: &mut Builder,
    ) -> Result<NodeId, BuilderError> {
        assert!(type_tag.is_none());
        if let Some(anchor) = anchor {
            if let Some(previous_anchor) = self.anchor() {
                return Err(BuilderError::MultipleAnchors(
                    anchor.to_owned(),
                    previous_anchor.to_owned(),
                ));
            }
            builder.add_spanned_value(self.with_anchor(anchor))
        } else {
            builder.add_spanned_value(self)
        }
    }
}

macro_rules! impl_int {
    ($($t:ty,)+) => {
        $(
            impl_int!($t);
        )*
    };
    ($t:ty) => {
        impl BuildValue for $t {
            fn build(
                self,
                anchor: Option<&str>,
                type_tag: Option<&str>,
                builder: &mut Builder,
            ) -> Result<NodeId, BuilderError> {
                assert!(type_tag.is_none());
                builder.add_scalar(OwnedScalar {
                    style: ScalarStyle::Plain,
                    value: itoa::Buffer::new().format(self).to_string(),
                    anchor: anchor.map(ToOwned::to_owned),
                })
            }
        }
    };
}

impl_int!(i8, i16, i32, i64, i128, isize, u8, u16, u32, u64, u128, usize,);

macro_rules! impl_float {
    ($($t:ty,)+) => {
        $(
            impl_float!($t);
        )*
    };
    ($t:ty) => {
        impl BuildValue for $t {
            fn build(
                self,
                anchor: Option<&str>,
                type_tag: Option<&str>,
                builder: &mut Builder,
            ) -> Result<NodeId, BuilderError> {
                assert!(type_tag.is_none());
                builder.add_scalar(OwnedScalar {
                    style: ScalarStyle::Plain,
                    value: ryu::Buffer::new().format(self).to_string(),
                    anchor: anchor.map(ToOwned::to_owned),
                })
            }
        }
    };
}

impl_float!(f32, f64,);

impl BuildValue for bool {
    fn build(
        self,
        anchor: Option<&str>,
        type_tag: Option<&str>,
        builder: &mut Builder,
    ) -> Result<NodeId, BuilderError> {
        assert!(type_tag.is_none());
        builder.add_scalar(OwnedScalar {
            style: ScalarStyle::Plain,
            value: if self { "true" } else { "false" }.to_owned(),
            anchor: anchor.map(ToOwned::to_owned),
        })
    }
}

/// Build a sequence from an iterator.
pub struct BuildAsList<I>(pub I);

impl<I> BuildValue for BuildAsList<I>
where
    I: IntoIterator,
    I::Item: BuildValue,
{
    fn build(
        self,
        anchor: Option<&str>,
        type_tag: Option<&str>,
        builder: &mut Builder,
    ) -> Result<NodeId, BuilderError> {
        let type_tag = type_tag
            .map(|tag| builder.add_scalar(OwnedScalar::plain(tag)))
            .transpose()?;

        let seq_id =
            builder.add_sequence(SequenceStyle::List, anchor.map(ToOwned::to_owned), type_tag)?;
        let mut seq_builder = SequenceBuilder {
            inner: MappingBuilder {
                node: Some(seq_id),
                builder,
            },
        };
        for item in self.0 {
            seq_builder.item(item)?;
        }
        Ok(seq_id)
    }
}

impl<I> BuildSeq for BuildAsList<I>
where
    I: IntoIterator,
    I::Item: BuildValue,
{
}

impl<T: BuildValue> BuildValue for Vec<T> {
    fn build(
        self,
        anchor: Option<&str>,
        type_tag: Option<&str>,
        builder: &mut Builder,
    ) -> Result<NodeId, BuilderError> {
        BuildAsList(self).build(anchor, type_tag, builder)
    }
}
impl<T: BuildValue> BuildSeq for Vec<T> {}

impl<T: BuildValue, const N: usize> BuildValue for [T; N] {
    fn build(
        self,
        anchor: Option<&str>,
        type_tag: Option<&str>,
        builder: &mut Builder,
    ) -> Result<NodeId, BuilderError> {
        BuildAsList(self).build(anchor, type_tag, builder)
    }
}
impl<T: BuildValue, const N: usize> BuildSeq for [T; N] {}

impl<T: BuildValue + Clone> BuildValue for &[T] {
    fn build(
        self,
        anchor: Option<&str>,
        type_tag: Option<&str>,
        builder: &mut Builder,
    ) -> Result<NodeId, BuilderError> {
        BuildAsList(self.iter().cloned()).build(anchor, type_tag, builder)
    }
}
impl<T: BuildValue + Clone> BuildSeq for &[T] {}

/// Build a tuple sequence from an iterator.
pub struct BuildAsTuple<I>(I);

impl<I> BuildValue for BuildAsTuple<I>
where
    I: IntoIterator,
    I::Item: BuildValue,
{
    fn build(
        self,
        anchor: Option<&str>,
        type_tag: Option<&str>,
        builder: &mut Builder,
    ) -> Result<NodeId, BuilderError> {
        let type_tag = type_tag
            .map(|tag| builder.add_scalar(OwnedScalar::plain(tag)))
            .transpose()?;

        let seq_id = builder.add_sequence(
            SequenceStyle::Tuple,
            anchor.map(ToOwned::to_owned),
            type_tag,
        )?;
        let mut seq_builder = SequenceBuilder {
            inner: MappingBuilder {
                node: Some(seq_id),
                builder,
            },
        };
        for item in self.0 {
            seq_builder.item(item)?;
        }
        Ok(seq_id)
    }
}

impl<I> BuildSeq for BuildAsTuple<I>
where
    I: IntoIterator,
    I::Item: BuildValue,
{
}

/// Build a mapping from an iterator.
pub struct BuildAsMapping<I>(I);

impl<I, A, B> BuildValue for BuildAsMapping<I>
where
    I: IntoIterator<Item = (A, B)>,
    A: BuildValue,
    B: BuildValue,
{
    fn build(
        self,
        anchor: Option<&str>,
        type_tag: Option<&str>,
        builder: &mut Builder,
    ) -> Result<NodeId, BuilderError> {
        let type_tag = type_tag
            .map(|tag| builder.add_scalar(OwnedScalar::plain(tag)))
            .transpose()?;

        let seq_id = builder.add_sequence(
            SequenceStyle::Mapping,
            anchor.map(ToOwned::to_owned),
            type_tag,
        )?;
        let mut builder = MappingBuilder {
            node: Some(seq_id),
            builder,
        };
        for (key, value) in self.0 {
            builder.entry(key, value)?;
        }
        Ok(seq_id)
    }
}

impl<I, A, B> BuildSeq for BuildAsMapping<I>
where
    I: IntoIterator<Item = (A, B)>,
    A: BuildValue,
    B: BuildValue,
{
}

impl<K, V> BuildValue for std::collections::BTreeMap<K, V>
where
    K: BuildValue,
    V: BuildValue,
{
    fn build(
        self,
        anchor: Option<&str>,
        type_tag: Option<&str>,
        builder: &mut Builder,
    ) -> Result<NodeId, BuilderError> {
        BuildAsMapping(self.into_iter().collect::<Vec<_>>()).build(anchor, type_tag, builder)
    }
}
impl<K: BuildValue, V: BuildValue> BuildSeq for std::collections::BTreeMap<K, V> {}

macro_rules! impl_tuple {
    ($($t:ident),*) => {
        impl<$($t: BuildValue),*> BuildValue for ($($t,)*) {
            #[allow(non_snake_case, unused)]
            fn build(
                self,
                anchor: Option<&str>,
                type_tag: Option<&str>,
                builder: &mut Builder,
            ) -> Result<NodeId, BuilderError> {
                let ($($t,)*) = self;

                let type_tag = type_tag
                .map(|tag| builder.add_scalar(OwnedScalar::plain(tag)))
                .transpose()?;

                let seq_id = builder.add_sequence(
                    SequenceStyle::Tuple,
                    anchor.map(ToOwned::to_owned),
                    type_tag,
                )?;
                let mut seq_builder = SequenceBuilder {
                    inner: MappingBuilder {
                        node: Some(seq_id),
                        builder,
                    },
                };
                $(
                    seq_builder.item($t)?;
                )*
                Ok(seq_id)
            }
        }
        impl<$($t: BuildValue),*> BuildSeq for ($($t,)*) {}
    };
}

impl_tuple!();
impl_tuple!(A);
impl_tuple!(A, B);
impl_tuple!(A, B, C);
impl_tuple!(A, B, C, D);
impl_tuple!(A, B, C, D, E);
impl_tuple!(A, B, C, D, E, F);
impl_tuple!(A, B, C, D, E, F, G);
impl_tuple!(A, B, C, D, E, F, G, H);
impl_tuple!(A, B, C, D, E, F, G, H, I);
impl_tuple!(A, B, C, D, E, F, G, H, I, J);
impl_tuple!(A, B, C, D, E, F, G, H, I, J, K);
impl_tuple!(A, B, C, D, E, F, G, H, I, J, K, L);
impl_tuple!(A, B, C, D, E, F, G, H, I, J, K, L, M);
impl_tuple!(A, B, C, D, E, F, G, H, I, J, K, L, M, N);
impl_tuple!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O);
impl_tuple!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P);

impl<F> BuildValue for F
where
    F: FnOnce(&mut MappingBuilder) -> Result<(), BuilderError>,
{
    fn build(
        self,
        anchor: Option<&str>,
        type_tag: Option<&str>,
        builder: &mut Builder,
    ) -> Result<NodeId, BuilderError> {
        let type_tag = type_tag
            .map(|tag| builder.add_scalar(OwnedScalar::plain(tag)))
            .transpose()?;
        let seq_id = builder.add_sequence(
            SequenceStyle::Mapping,
            anchor.map(ToOwned::to_owned),
            type_tag,
        )?;
        let mut builder = MappingBuilder {
            node: Some(seq_id),
            builder,
        };
        self(&mut builder)?;
        Ok(seq_id)
    }
}

impl<F> BuildSeq for F where F: FnOnce(&mut MappingBuilder) -> Result<(), BuilderError> {}
