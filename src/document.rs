use std::{collections::HashMap, io::BufRead, num::NonZeroUsize};

use crate::{
    Builder, Error, MappingBuilder, OwnedScalar, ParseStream, Scalar, ScalarStyle, Sequence,
    SequenceStyle, Span, Spanned, SpannedExt, SpannedSequence, SpannedValue, Value,
};

pub struct Document {
    pub(crate) nodes: Vec<Node>,
    pub(crate) sequence: SequenceNode,
    pub(crate) anchors: HashMap<String, NodeId>,
    pub(crate) span: Span,
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
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct Node {
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
                    | NodeData::Scalar(OwnedScalar {
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
pub(crate) enum NodeData {
    Scalar(OwnedScalar),
    Sequence(SequenceNode),
}

#[derive(Clone, Debug)]
pub(crate) struct SequenceNode {
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct SequenceItem {
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
    fn get_node<B: ValueReader>(&self, id: NodeId) -> B::Output<'_> {
        let node = &self.nodes[id];
        let anchor = node.anchor.as_ref().map(|anchor| Spanned {
            value: &*anchor.value,
            span: anchor.span,
        });

        match &node.data {
            NodeData::Scalar(scalar) => B::scalar(scalar.borrow().in_span(node.span)),
            NodeData::Sequence(sequence) => {
                let type_tag = if let Some(type_tag) = sequence.type_tag {
                    let type_tag_node = &self.nodes[type_tag];
                    if let NodeData::Scalar(ref scalar) = type_tag_node.data {
                        Some(Spanned {
                            value: scalar.borrow(),
                            span: type_tag_node.span,
                        })
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

    fn add_node_to_sequence<'a, B: ValueReader>(
        &'a self,
        item: &SequenceItem,
        seq: &mut B::Seq<'a>,
    ) {
        let key = item.key_node.map(|key| self.get_node::<B>(key));
        let value = self.get_node::<B>(item.value_node);
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
            .map(|&id| self.get_node::<BareValueReader>(id))
    }

    #[inline]
    pub fn get_spanned_tag(&self, tag: &str) -> Option<SpannedValue> {
        self.anchors
            .get(tag)
            .map(|&id| self.get_node::<SpannedValueReader>(id))
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[doc(hidden)]
pub struct NodeId(pub(crate) NonZeroUsize);

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
