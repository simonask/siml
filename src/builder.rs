use std::io::BufRead;

use crate::{
    private::*, CachedEvent, Document, Error, Event, NodeId, ParseStream, Scalar, ScalarStyle,
    ScannerError, SequenceStyle, Spanned, SpannedExt, Value,
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
}

impl Builder {
    pub fn build(&mut self) -> Result<Document, BuilderError> {
        self.doc.check_complete()?;
        Ok(std::mem::take(&mut self.doc))
    }

    fn merge(&mut self, target: Option<NodeId>, anchor: &str) -> Result<(), BuilderError> {
        let source = self.doc.resolve_anchor(anchor)?;
        self.doc.merge_nodes(target, source);
        Ok(())
    }

    /// Add a key-value entry to the root mapping.
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

    /// Add an item without a key to the root mapping.
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
        let mut pending_key: Option<Option<NodeId>> = None;

        let mut event = CachedEvent::default();
        loop {
            parser.next_event(&mut event)?;
            let Some(event) = event.get() else {
                if !is_root {
                    return Err(ScannerError::UnexpectedEof.into());
                } else {
                    return Ok(());
                }
            };

            let key_or_value = match event {
                Event::Scalar(scalar) => Some(self.builder.doc.add_spanned_scalar(scalar)?),
                Event::Ref(anchor) => Some(self.builder.doc.resolve_anchor(&anchor)?),
                Event::Merge(anchor) => {
                    if pending_key.is_some() {
                        return Err(BuilderError::UnexpectedEvent.into());
                    }
                    self.builder.merge(self.node, &anchor)?;
                    continue;
                }
                Event::BeginSequence {
                    span,
                    style,
                    type_tag,
                    anchor,
                } => {
                    let type_tag = type_tag
                        .map(|type_tag| self.builder.doc.add_spanned_scalar(type_tag))
                        .transpose()?;

                    let seq_id = self
                        .builder
                        .doc
                        .add_spanned_sequence(style, anchor, type_tag, span)?;
                    let mut seq_builder = SequenceBuilder {
                        inner: MappingBuilder {
                            node: Some(seq_id),
                            builder: self.builder,
                        },
                    };
                    seq_builder.parse(parser)?;
                    Some(seq_id)
                }
                Event::EndSequence(_) => return Err(BuilderError::UnexpectedEvent.into()),
                Event::BeginMapping {
                    span,
                    type_tag,
                    anchor,
                } => {
                    let type_tag = type_tag
                        .map(|type_tag| self.builder.doc.add_spanned_scalar(type_tag))
                        .transpose()?;

                    let submap_id = self.builder.doc.add_spanned_sequence(
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
                    Some(submap_id)
                }
                Event::EndMapping(_) => {
                    if pending_key.is_some() {
                        return Err(BuilderError::UnexpectedEvent.into());
                    }
                    return Ok(());
                }
                Event::Empty(span) => {
                    if pending_key.is_some() {
                        Some(
                            self.builder
                                .doc
                                .add_spanned_scalar(Scalar::plain("").in_span(span))?,
                        )
                    } else {
                        None
                    }
                }
                Event::Comment(_) => continue,
            };

            if let Some(key) = pending_key.take() {
                self.seq().items.push(SequenceItem {
                    key_node: key,
                    value_node: key_or_value.expect("no value for key"),
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
            self.builder.doc.complete_sequence(node);
        }
    }
}

impl<'b> SequenceBuilder<'b> {
    fn parse<R: BufRead>(&mut self, parser: &mut ParseStream<R>) -> Result<(), Error> {
        let mut event = CachedEvent::default();
        loop {
            parser.next_event(&mut event)?;
            let Some(event) = event.get() else {
                return Err(ScannerError::UnexpectedEof.into());
            };

            let value = match event {
                Event::Scalar(scalar) => self.inner.builder.doc.add_spanned_scalar(scalar)?,
                Event::Ref(anchor) => self.inner.builder.doc.resolve_anchor(&anchor)?,
                Event::Merge(anchor) => {
                    self.inner.builder.merge(self.inner.node, &anchor)?;
                    continue;
                }
                Event::BeginSequence {
                    span,
                    style,
                    type_tag,
                    anchor,
                } => {
                    let type_tag = type_tag
                        .map(|type_tag| self.inner.builder.doc.add_spanned_scalar(type_tag))
                        .transpose()?;

                    let seq_id = self
                        .inner
                        .builder
                        .doc
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
                Event::EndSequence(span) => {
                    // TODO: Merge spans
                    return Ok(());
                }
                Event::BeginMapping {
                    span,
                    type_tag,
                    anchor,
                } => {
                    let type_tag = type_tag
                        .map(|type_tag| self.inner.builder.doc.add_spanned_scalar(type_tag))
                        .transpose()?;

                    let submap_id = self.inner.builder.doc.add_spanned_sequence(
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
                Event::EndMapping(_) => return Err(BuilderError::UnexpectedEvent.into()),
                Event::Empty(span) => self.inner.builder.doc.add_spanned_scalar(Spanned {
                    value: Scalar::plain(""),
                    span,
                })?,
                Event::Comment(_) => continue,
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
        builder.doc.add_scalar_infer_style(self, anchor)
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
        builder.doc.add_scalar_infer_style(&self, anchor)
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
        builder
            .doc
            .add_scalar_infer_style(&self.to_string(), anchor)
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
            builder.doc.add_value(&self.with_anchor(anchor))
        } else {
            builder.doc.add_value(&self)
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
                builder.doc.add_scalar(Scalar {
                    style: ScalarStyle::Plain,
                    value: itoa::Buffer::new().format(self),
                    anchor,
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
                builder.doc.add_scalar(Scalar {
                    style: ScalarStyle::Plain,
                    value: ryu::Buffer::new().format(self),
                    anchor,
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
        builder.doc.add_scalar(Scalar {
            style: ScalarStyle::Plain,
            value: if self { "true" } else { "false" },
            anchor,
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
            .map(|tag| builder.doc.add_scalar(Scalar::plain(tag)))
            .transpose()?;

        let seq_id = builder
            .doc
            .add_sequence(SequenceStyle::List, anchor, type_tag)?;
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
            .map(|tag| builder.doc.add_scalar(Scalar::plain(tag)))
            .transpose()?;

        let seq_id = builder
            .doc
            .add_sequence(SequenceStyle::Tuple, anchor, type_tag)?;
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
            .map(|tag| builder.doc.add_scalar(Scalar::plain(tag)))
            .transpose()?;

        let seq_id = builder
            .doc
            .add_sequence(SequenceStyle::Mapping, anchor, type_tag)?;
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
                .map(|tag| builder.doc.add_scalar(Scalar::plain(tag)))
                .transpose()?;

                let seq_id = builder.doc.add_sequence(
                    SequenceStyle::Tuple,
                    anchor,
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
            .map(|tag| builder.doc.add_scalar(Scalar::plain(tag)))
            .transpose()?;
        let seq_id = builder
            .doc
            .add_sequence(SequenceStyle::Mapping, anchor, type_tag)?;
        let mut builder = MappingBuilder {
            node: Some(seq_id),
            builder,
        };
        self(&mut builder)?;
        Ok(seq_id)
    }
}

impl<F> BuildSeq for F where F: FnOnce(&mut MappingBuilder) -> Result<(), BuilderError> {}
