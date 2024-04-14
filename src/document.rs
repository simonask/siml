use std::{
    collections::{hash_map, HashMap, HashSet},
    io::BufRead,
    num::NonZeroUsize,
};

use crate::{
    builder::MappingBuilder, Builder, BuilderError, Error, ParseStream, Scalar, ScalarStyle,
    Sequence, SequenceStyle, Span, Spanned, SpannedExt, SpannedSequence, SpannedValue, Value,
};

pub struct Document {
    pub(crate) nodes: Vec<Node>,
    pub(crate) sequence: SequenceNode,
    pub(crate) anchors: HashMap<String, NodeId>,
    pub(crate) span: Span,
    incomplete_sequences: HashSet<NodeId>,
    incomplete_anchors: HashSet<String>,
}

impl Default for Document {
    fn default() -> Self {
        Self {
            nodes: vec![],
            sequence: SequenceNode {
                style: SequenceStyle::Mapping,
                type_tag: None,
                items: vec![],
            },
            anchors: HashMap::default(),
            span: Span::default(),
            incomplete_sequences: HashSet::default(),
            incomplete_anchors: HashSet::default(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Node {
    pub anchor: Option<Spanned<String>>,
    pub data: NodeData,
    pub span: Span,
}

impl Node {
    pub fn is_complex_key(&self) -> bool {
        self.anchor.is_some()
            || matches!(
                self.data,
                NodeData::Sequence(_)
                    | NodeData::Scalar(ScalarNode {
                        style: ScalarStyle::QuotedTrimIndent | ScalarStyle::QuotedTrimWhitespace,
                        ..
                    })
            )
    }

    /// When this node is part of a sequence, returns true if it would prefer
    /// its container to be split into multiple lines.
    pub fn is_long_form_item(&self) -> bool {
        if self.anchor.is_some() {
            return true;
        }

        match self.data {
            NodeData::Scalar(ref scalar) => {
                matches!(scalar.style, ScalarStyle::QuotedTrimIndent) || scalar.value.len() >= 20
            }
            NodeData::Sequence(_) => true,
        }
    }
}

#[derive(Clone, Debug)]
pub enum NodeData {
    Scalar(ScalarNode),
    Sequence(SequenceNode),
}

#[derive(Clone, Debug)]
pub struct SequenceNode {
    pub style: SequenceStyle,
    pub type_tag: Option<NodeId>,
    pub items: Vec<SequenceItem>,
}

impl SequenceNode {
    #[inline]
    pub fn has_keys(&self) -> bool {
        self.items.iter().any(|item| item.key_node.is_some())
    }
}

#[derive(Clone, Debug)]
pub struct ScalarNode {
    pub style: ScalarStyle,
    pub value: String,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SequenceItem {
    pub key_node: Option<NodeId>,
    pub value_node: NodeId,
}

trait ValueReader {
    type Output<'r>: Sized + 'r;
    type Seq<'r>: Sized + 'r;

    fn new_seq<'r>(
        anchor: Option<Spanned<&'r str>>,
        type_tag: Option<Spanned<Scalar<'r>>>,
        capacity: usize,
        style: SequenceStyle,
        span: Span,
    ) -> Self::Seq<'r>;

    fn seq_push<'r>(
        seq: &mut Self::Seq<'r>,
        key: Option<Self::Output<'r>>,
        value: Self::Output<'r>,
    );

    fn scalar<'r>(scalar: Spanned<Scalar<'r>>) -> Self::Output<'r>;
    fn seq<'r>(seq: Self::Seq<'r>) -> Self::Output<'r>;
}

struct BareValueReader;
impl ValueReader for BareValueReader {
    type Output<'r> = Value<'r>;
    type Seq<'r> = Sequence<'r>;

    #[inline]
    fn new_seq<'r>(
        anchor: Option<Spanned<&'r str>>,
        type_tag: Option<Spanned<Scalar<'r>>>,
        capacity: usize,
        style: SequenceStyle,
        _span: Span,
    ) -> Self::Seq<'r> {
        Sequence {
            anchor: anchor.map(Spanned::into_inner),
            type_tag: type_tag.map(Spanned::into_inner),
            style,
            items: Vec::with_capacity(capacity),
        }
    }

    #[inline]
    fn seq_push<'r>(
        seq: &mut Self::Seq<'r>,
        key: Option<Self::Output<'r>>,
        value: Self::Output<'r>,
    ) {
        seq.items.push((key, value));
    }

    #[inline]
    fn scalar<'r>(scalar: Spanned<Scalar<'r>>) -> Self::Output<'r> {
        Value::Scalar(scalar.into_inner())
    }

    #[inline]
    fn seq<'r>(seq: Self::Seq<'r>) -> Self::Output<'r> {
        Value::Sequence(seq)
    }
}

struct SpannedValueReader;
impl ValueReader for SpannedValueReader {
    type Output<'r> = SpannedValue<'r>;
    type Seq<'r> = SpannedSequence<'r>;

    #[inline]
    fn new_seq<'r>(
        anchor: Option<Spanned<&'r str>>,
        type_tag: Option<Spanned<Scalar<'r>>>,
        capacity: usize,
        style: SequenceStyle,
        span: Span,
    ) -> Self::Seq<'r> {
        SpannedSequence {
            span,
            anchor,
            type_tag,
            style,
            items: Vec::with_capacity(capacity),
        }
    }

    #[inline]
    fn seq_push<'r>(
        seq: &mut Self::Seq<'r>,
        key: Option<Self::Output<'r>>,
        value: Self::Output<'r>,
    ) {
        seq.items.push((key, value));
    }

    #[inline]
    fn scalar<'r>(scalar: Spanned<Scalar<'r>>) -> Self::Output<'r> {
        SpannedValue::Scalar(scalar)
    }

    #[inline]
    fn seq<'r>(seq: Self::Seq<'r>) -> Self::Output<'r> {
        SpannedValue::Sequence(seq)
    }
}

impl Document {
    fn read_node<B: ValueReader>(&self, id: NodeId) -> B::Output<'_> {
        let node = &self.nodes[id];
        let anchor = node.anchor.as_ref().map(|anchor| Spanned {
            value: &*anchor.value,
            span: anchor.span,
        });

        match &node.data {
            NodeData::Scalar(ScalarNode { value, style }) => B::scalar(
                Scalar {
                    value: value.as_str(),
                    style: *style,
                    anchor: anchor.map(Spanned::into_inner),
                }
                .in_span(node.span),
            ),
            NodeData::Sequence(sequence) => {
                let type_tag = if let Some(type_tag) = sequence.type_tag {
                    let type_tag_node = &self.nodes[type_tag];
                    if let NodeData::Scalar(ScalarNode {
                        ref value,
                        ref style,
                    }) = type_tag_node.data
                    {
                        Some(Scalar::new(value.as_str(), *style).in_span(type_tag_node.span))
                    } else {
                        unreachable!("type tag is not a scalar node")
                    }
                } else {
                    None
                };

                let mut seq = B::new_seq(
                    anchor,
                    type_tag,
                    sequence.items.len(),
                    sequence.style,
                    node.span,
                );
                for item in sequence.items.iter() {
                    self.add_node_to_sequence::<B>(item, &mut seq);
                }
                B::seq(seq)
            }
        }
    }

    #[inline]
    pub fn get_root(&self) -> &SequenceNode {
        &self.sequence
    }

    #[inline]
    pub fn get_node(&self, node_id: NodeId) -> &Node {
        &self.nodes[node_id]
    }

    #[inline]
    pub fn get_node_mut(&mut self, node_id: NodeId) -> &mut Node {
        &mut self.nodes[node_id]
    }

    fn add_node_to_sequence<'a, B: ValueReader>(
        &'a self,
        item: &SequenceItem,
        seq: &mut B::Seq<'a>,
    ) {
        let key = item.key_node.map(|key| self.read_node::<B>(key));
        let value = self.read_node::<B>(item.value_node);
        B::seq_push(seq, key, value);
    }

    pub fn to_sequence(&self) -> Sequence {
        let mut seq = Sequence {
            anchor: None,
            type_tag: None,
            style: SequenceStyle::Mapping,
            items: Vec::with_capacity(self.sequence.items.len()),
        };
        for item in self.sequence.items.iter() {
            self.add_node_to_sequence::<BareValueReader>(item, &mut seq);
        }
        seq
    }

    #[inline]
    pub fn to_value(&self) -> Value {
        Value::Sequence(self.to_sequence())
    }

    pub fn to_spanned_sequence(&self) -> SpannedSequence {
        let mut seq = SpannedSequence {
            anchor: None,
            type_tag: None,
            style: SequenceStyle::Mapping,
            items: Vec::with_capacity(self.sequence.items.len()),
            span: self.span,
        };
        for item in self.sequence.items.iter() {
            self.add_node_to_sequence::<SpannedValueReader>(item, &mut seq);
        }
        seq
    }

    #[inline]
    pub fn to_spanned_value(&self) -> SpannedValue {
        SpannedValue::Sequence(self.to_spanned_sequence())
    }

    #[inline]
    pub fn get_tag(&self, tag: &str) -> Option<Value> {
        self.anchors
            .get(tag)
            .map(|&id| self.read_node::<BareValueReader>(id))
    }

    #[inline]
    pub fn get_spanned_tag(&self, tag: &str) -> Option<SpannedValue> {
        self.anchors
            .get(tag)
            .map(|&id| self.read_node::<SpannedValueReader>(id))
    }

    pub fn parse<R: BufRead>(reader: R) -> Result<Self, Error> {
        let mut parser = ParseStream::new(reader);
        Self::from_parser(&mut parser)
    }

    pub fn from_parser<R: BufRead>(parser: &mut ParseStream<R>) -> Result<Self, Error> {
        let mut builder = Builder::default();
        let mut mapping_builder = MappingBuilder {
            node: None,
            builder: &mut builder,
        };
        mapping_builder.parse_root(parser)?;
        std::mem::drop(mapping_builder);
        builder.build().map_err(Into::into)
    }

    pub(crate) fn set_anchor(
        &mut self,
        id: NodeId,
        anchor: Option<&str>,
    ) -> Result<(), BuilderError> {
        let Some(anchor) = anchor else { return Ok(()) };

        match self.anchors.entry(anchor.to_owned()) {
            hash_map::Entry::Occupied(entry) => {
                return Err(BuilderError::DuplicateAnchor(entry.key().clone()));
            }
            hash_map::Entry::Vacant(entry) => {
                if self.incomplete_anchors.contains(entry.key()) {
                    return Err(BuilderError::DuplicateAnchor(entry.key().clone()));
                }
                entry.insert(id);
                Ok(())
            }
        }
    }

    pub(crate) fn add_spanned_scalar(
        &mut self,
        scalar: Spanned<Scalar>,
    ) -> Result<NodeId, BuilderError> {
        let node_id = NodeId(NonZeroUsize::new(self.nodes.len() + 1).unwrap());

        self.set_anchor(node_id, scalar.anchor.clone())?;

        self.nodes.push(Node {
            anchor: scalar
                .anchor
                .clone()
                .map(|anchor| anchor.to_owned().in_span(Span::default())),
            data: NodeData::Scalar(ScalarNode {
                style: scalar.value.style,
                value: scalar.value.value.to_owned(),
            }),
            span: scalar.span,
        });

        Ok(node_id)
    }

    /// Add a scalar to the document, inferring the best style based on its contents.
    ///
    /// See also [`add_scalar()`].
    #[inline]
    pub fn add_scalar_infer_style(
        &mut self,
        value: &str,
        anchor: Option<&str>,
    ) -> Result<NodeId, BuilderError> {
        self.add_spanned_scalar(
            Scalar::infer_style(value)
                .with_anchor(anchor)
                .to_owned()
                .in_span(Span::default()),
        )
    }

    /// Add a scalar node to the document.
    ///
    /// Note: This does not insert the scalar anywhere in the document's node
    /// hierarchy. It must still be added to a sequence or mapping, or the
    /// document root mapping.
    ///
    /// This function's purpose is to support higher-level builder APIs.
    ///
    /// Use [`Builder`](crate::Builder) to have these details handled
    /// automatically.
    #[inline]
    pub fn add_scalar(&mut self, scalar: Scalar) -> Result<NodeId, BuilderError> {
        self.add_spanned_scalar(scalar.to_owned().in_span(Span::default()))
    }

    pub(crate) fn add_spanned_sequence(
        &mut self,
        style: SequenceStyle,
        anchor: Option<Spanned<&str>>,
        type_tag: Option<NodeId>,
        span: Span,
    ) -> Result<NodeId, BuilderError> {
        let node_id = NodeId(NonZeroUsize::new(self.nodes.len() + 1).unwrap());

        if let Some(tag) = anchor.clone().map(Spanned::into_inner) {
            self.set_anchor(node_id, Some(tag))?;
            self.incomplete_anchors.insert(tag.to_owned());
        }
        self.incomplete_sequences.insert(node_id);

        self.nodes.push(Node {
            anchor: anchor.map(|anchor| anchor.map(ToOwned::to_owned)),
            data: NodeData::Sequence(SequenceNode {
                style,
                type_tag,
                items: vec![],
            }),
            span,
        });

        Ok(node_id)
    }

    /// Add a sequence or mapping node to the document.
    ///
    /// When the sequence is done being built, `complete_sequence()` must be
    /// called.
    ///
    /// Note: This does not insert the sequence anywhere in the document's node
    /// hierarchy. It must still be added to a sequence or mapping, or the
    /// document root mapping.
    ///
    /// Note: If the sequence has an anchor, the anchor cannot be referenced
    /// before `complete_sequence()` is called.
    ///
    /// This function's purpose is to support higher-level builder APIs.
    ///
    /// Use [`Builder`](crate::Builder) to have these details handled
    /// automatically.
    #[inline]
    pub fn add_sequence(
        &mut self,
        style: SequenceStyle,
        anchor: Option<&str>,
        type_tag: Option<NodeId>,
    ) -> Result<NodeId, BuilderError> {
        self.add_spanned_sequence(
            style,
            anchor.map(|anchor| anchor.in_span(Span::default())),
            type_tag,
            Span::default(),
        )
    }

    /// Complete a previously added sequence.
    ///
    /// After calling this, the sequence becomes immutable, and can be
    /// referenced by other nodes.
    ///
    /// See [`add_sequence()`].
    pub fn complete_sequence(&mut self, node: NodeId) {
        self.incomplete_sequences.remove(&node);
        if let Some(tag) = self.nodes[node].anchor.as_ref() {
            let removed = self.incomplete_anchors.remove(tag.value.as_str());
            assert!(removed, "anchor was not marked as incomplete; perhaps this NodeId is a scalar, or it belongs to a different document?");
        }
    }

    pub(crate) fn merge_span(&mut self, node: Option<NodeId>, span: Span) {
        if let Some(node) = node {
            self.nodes[node].span = self.nodes[node].span.merge(&span);
        } else {
            self.span = self.span.merge(&span);
        }
    }

    /// Add a node as the child of a sequence.
    ///
    /// If `seq` is None, the key-value pair is added to the document's root.
    ///
    /// If `key_node` is None, the value is added as a "keyless" item in a
    /// mapping, or as a regular element in a list/tuple. This function may be
    /// called with any type of sequence node (lists, tuples, and mappings).
    pub fn add_sequence_item(
        &mut self,
        seq: Option<NodeId>,
        key_node: Option<NodeId>,
        value_node: NodeId,
    ) {
        if key_node.is_some_and(|key_node| self.incomplete_sequences.contains(&key_node)) {
            panic!("cannot use an incomplete sequence as a key in a mapping");
        }
        if self.incomplete_sequences.contains(&value_node) {
            panic!("cannot add an incomplete node to another mapping or sequence");
        }

        if let Some(seq) = seq {
            if !self.incomplete_sequences.contains(&seq) {
                panic!("cannot add nodes to sequences that have already been completed");
            }

            if let NodeData::Sequence(ref mut seq) = self.nodes[seq].data {
                seq.items.push(SequenceItem {
                    key_node,
                    value_node,
                });
            } else {
                panic!("not a sequence node")
            }
        } else {
            self.sequence.items.push(SequenceItem {
                key_node,
                value_node,
            })
        }
    }

    /// Check that there are no incomplete sequences in the document.
    ///
    /// Missing calls to `complete_sequence()` is considered a logic error, and
    /// this function may panic instead of returning a `BuilderError`.
    pub fn check_complete(&self) -> Result<(), BuilderError> {
        if let Some(incomplete) = self.incomplete_anchors.iter().next() {
            return Err(BuilderError::IncompleteAnchor(incomplete.clone()));
        }
        if let Some(_incomplete) = self.incomplete_sequences.iter().next() {
            panic!("add_sequence() was called without a corresponding call to complete_sequence()");
        }
        Ok(())
    }

    /// Add any value to the document.
    ///
    /// Note: This does not insert the value anywhere in the document's node
    /// hierarchy. It must still be added to a sequence or mapping, or the
    /// document root mapping.
    ///
    /// If an error occurs while inserting values (such as if a duplicate anchor
    /// exists), the document will be left in the same state as before this
    /// call. In other words, composite values will not be partially inserted.
    ///
    /// This function's purpose is to support higher-level builder APIs.
    ///
    /// Use [`Builder`](crate::Builder) to have these details handled
    /// automatically.
    pub fn add_value(&mut self, value: &Value) -> Result<NodeId, BuilderError> {
        match value {
            Value::Scalar(scalar) => self.add_scalar(*scalar),
            Value::Sequence(seq) => {
                let type_tag = seq.type_tag.map(|t| self.add_scalar(t)).transpose()?;
                let id = self.add_sequence(seq.style, seq.anchor, type_tag)?;
                let mut err = None;
                for (k, v) in seq.items.iter() {
                    let key_node = if let Some(k) = k {
                        match self.add_value(k) {
                            Ok(key_node) => Some(key_node),
                            Err(e) => {
                                err = Some(e);
                                break;
                            }
                        }
                    } else {
                        None
                    };
                    let value_node = match self.add_value(v) {
                        Ok(value_node) => value_node,
                        Err(e) => {
                            err = Some(e);
                            break;
                        }
                    };
                    self.add_sequence_item(Some(id), key_node, value_node);
                }
                self.complete_sequence(id);
                if let Some(err) = err {
                    self.remove_from(id);
                    Err(err)
                } else {
                    Ok(id)
                }
            }
        }
    }

    fn remove_from(&mut self, id: NodeId) {
        self.nodes.splice(id.0.get() - 1.., []);
    }

    /// Get the node ID for the given anchor.
    ///
    /// This function returns an error if the anchor is not defined, or if the
    /// anchor refers to an incomplete sequence.
    pub fn resolve_anchor(&self, anchor: &str) -> Result<NodeId, BuilderError> {
        if self.incomplete_anchors.contains(anchor) {
            return Err(BuilderError::IncompleteAnchor(anchor.to_owned()));
        }
        if let Some(node) = self.anchors.get(anchor) {
            Ok(*node)
        } else {
            Err(BuilderError::UndefinedAnchor(anchor.to_owned()))
        }
    }

    /// Given two sequence nodes, copy all items from one to the other.
    ///
    /// If `into` is None, the items from `from` are copied into the document's
    /// root.
    pub fn merge_nodes(&mut self, into: Option<NodeId>, from: NodeId) {
        assert_ne!(into, Some(from), "cannot merge a node into itself");
        if self.incomplete_sequences.contains(&from) {
            panic!("cannot merge an incomplete sequence into another sequence");
        }

        let (src, dst) = if let Some(target) = into {
            // TODO: Replace this with slice::get_many_mut when stablex
            let src_idx = from.0.get() - 1;
            let dst_idx = target.0.get() - 1;
            let (src, dst_node) = if src_idx < dst_idx {
                let (lower, upper) = self.nodes.split_at_mut(dst_idx);
                (&mut lower[src_idx], &mut upper[0])
            } else {
                let (lower, upper) = self.nodes.split_at_mut(src_idx);
                (&mut upper[0], &mut lower[dst_idx])
            };
            if let NodeData::Sequence(dst_seq) = &mut dst_node.data {
                (src, dst_seq)
            } else {
                panic!("cannot merge into non-sequence")
            }
        } else {
            (&mut self.nodes[from], &mut self.sequence)
        };

        if let NodeData::Sequence(src_seq) = &src.data {
            for item in src_seq.items.iter() {
                dst.items.push(item.clone());
            }
        } else {
            dst.items.push(SequenceItem {
                key_node: None,
                value_node: from,
            });
        }
    }
}

impl PartialEq<Value<'_>> for Document {
    fn eq(&self, other: &Value<'_>) -> bool {
        match other {
            Value::Sequence(seq) => self.to_sequence() == *seq,
            _ => false,
        }
    }
}

impl PartialEq<SpannedValue<'_>> for Document {
    fn eq(&self, other: &SpannedValue<'_>) -> bool {
        match other {
            SpannedValue::Sequence(seq) => self.to_spanned_sequence() == *seq,
            _ => false,
        }
    }
}

/// The ID of a node in a document.
///
/// Node IDs are only valid for a particular document, and cannot be reused.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[doc(hidden)]
pub struct NodeId(pub NonZeroUsize);

impl std::ops::Index<NodeId> for Vec<Node> {
    type Output = Node;

    #[inline]
    fn index(&self, index: NodeId) -> &Self::Output {
        self.get(index.0.get() - 1)
            .expect("invalid node ID in document")
    }
}

impl std::ops::IndexMut<NodeId> for Vec<Node> {
    #[inline]
    fn index_mut(&mut self, index: NodeId) -> &mut Self::Output {
        self.get_mut(index.0.get() - 1)
            .expect("invalid node ID in document")
    }
}

impl std::fmt::Debug for Document {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.to_value(), f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_basic() {
        let input = r#"
            foo: bar
            baz: 42
            seq: [
              1,
              2,
              3
            ]
        "#;
        let doc = Document::parse(input.as_bytes()).unwrap();

        assert_eq!(
            doc.to_value(),
            Value::mapping([
                (Some(Value::plain("foo")), Value::plain("bar")),
                (Some(Value::plain("baz")), Value::plain("42")),
                (
                    Some(Value::plain("seq")),
                    Value::seq([Value::plain("1"), Value::plain("2"), Value::plain("3"),])
                ),
            ])
        );
    }

    #[test]
    fn parse_basic_anchors() {
        let input = r#"
            foo: bar
            ? @baz baz: @forty-two 42
            seq: [
              1,
              2,
              3
            ]
        "#;
        let doc = Document::parse(input.as_bytes()).unwrap();

        assert_eq!(
            doc.to_value(),
            Value::mapping([
                (Some(Value::plain("foo")), Value::plain("bar")),
                (
                    Some(Value::plain("baz").with_anchor("baz")),
                    Value::plain("42").with_anchor("forty-two")
                ),
                (
                    Some(Value::plain("seq")),
                    Value::seq([Value::plain("1"), Value::plain("2"), Value::plain("3"),])
                ),
            ])
        );
    }
}
