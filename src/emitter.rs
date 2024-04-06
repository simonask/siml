use std::collections::HashMap;

use crate::{
    CharExt, Document, Node, NodeData, NodeId, ScalarStyle, SequenceItem, SequenceNode,
    SequenceStyle,
};

pub struct Emitter<W> {
    writer: W,
    state: State,
}

#[derive(Default)]
struct State {
    emitted_anchors: HashMap<NodeId, String>,
}

impl State {
    fn get_anchor(&self, node_id: NodeId) -> Option<&str> {
        self.emitted_anchors.get(&node_id).map(String::as_str)
    }

    fn set_anchor(&mut self, node_id: NodeId, anchor: String) {
        self.emitted_anchors.insert(node_id, anchor);
    }
}

mod private {
    pub struct FmtWriter<W>(pub W);
    impl<W: std::io::Write> std::fmt::Write for FmtWriter<W> {
        #[inline]
        fn write_str(&mut self, s: &str) -> std::fmt::Result {
            self.0
                .write(s.as_bytes())
                .map_err(|_| std::fmt::Error)
                .map(|_| ())
        }
    }
}

impl<W: std::io::Write> Emitter<private::FmtWriter<W>> {
    pub fn with_io_writer(writer: W) -> Self
    where
        W: std::io::Write,
    {
        Self {
            writer: private::FmtWriter(writer),
            state: State::default(),
        }
    }
}

impl<W: std::fmt::Write> Emitter<W> {
    pub fn new(writer: W) -> Self {
        Self {
            writer,
            state: State::default(),
        }
    }

    pub fn emit(mut self, document: &Document) -> Result<(), std::fmt::Error> {
        LargeMappingBodyEmitter {
            state: &mut self.state,
            doc: document,
            seq: &document.sequence,
            indent: 0,
            need_newlines: 0,
            did_emit_items: false,
        }
        .emit(&mut self.writer)
    }
}

fn write_newline(writer: &mut impl std::fmt::Write, indent: usize) -> Result<(), std::fmt::Error> {
    writeln!(writer)?;
    for _ in 0..indent {
        writer.write_str("  ")?;
    }
    Ok(())
}

/// Emit any value in a sensible way.
struct ValueEmitter<'a> {
    state: &'a mut State,
    doc: &'a Document,
    node: NodeId,
    indent: usize,
}

impl<'a> ValueEmitter<'a> {
    fn emit(self, writer: &mut impl std::fmt::Write) -> Result<(), std::fmt::Error> {
        let node = &self.doc.nodes[self.node];
        match node.data {
            NodeData::Scalar(_) => ScalarEmitter {
                state: self.state,
                doc: self.doc,
                node: self.node,
                indent: self.indent,
            }
            .emit(writer),
            NodeData::Sequence(ref seq) => SequenceEmitter {
                state: self.state,
                doc: self.doc,
                node: self.node,
                seq,
                indent: self.indent,
            }
            .emit(writer),
        }
    }
}

struct SequenceEmitter<'a> {
    state: &'a mut State,
    doc: &'a Document,
    node: NodeId,
    seq: &'a SequenceNode,
    indent: usize,
}

impl<'a> SequenceEmitter<'a> {
    fn emit(self, writer: &mut impl std::fmt::Write) -> Result<(), std::fmt::Error> {
        let node = &self.doc.nodes[self.node];

        // If the node has an anchor, register the anchor if this is the first
        // time we see it, otherwise emit a reference to the previous node.
        if let Some(anchor) = node.anchor.as_deref() {
            if let Some(reference) = self.state.get_anchor(self.node) {
                writer.write_char('&')?;
                writer.write_str(reference)?;
                return Ok(());
            }

            self.state.set_anchor(self.node, anchor.clone());
            write!(writer, "@{anchor} ")?;
        }

        let style = match self.seq.style {
            SequenceStyle::Mapping => SequenceStyle::Mapping,
            style => {
                if self.seq.has_keys() {
                    SequenceStyle::Mapping
                } else {
                    style
                }
            }
        };

        match style {
            SequenceStyle::Mapping => self.emit_mapping(writer),
            SequenceStyle::List => self.emit_list(writer),
            SequenceStyle::Tuple => self.emit_tuple(writer),
        }
    }

    fn emit_type_tag(
        &mut self,
        writer: &mut impl std::fmt::Write,
        is_tuple: bool,
    ) -> Result<(), std::fmt::Error> {
        if let Some(type_tag) = self.seq.type_tag {
            let node = &self.doc.nodes[type_tag];
            let scalar = match &node.data {
                NodeData::Scalar(scalar) => scalar.borrow(),
                _ => unreachable!("expected scalar node (type tag must be a scalar)"),
            };
            let required_style = ScalarStyle::infer(scalar.value);
            if required_style == ScalarStyle::Plain {
                writer.write_str(scalar.value)?;
            } else {
                write_dquoted_scalar(scalar.value, writer)?;
            }
            if !is_tuple {
                writer.write_char(' ')?;
            }
        }
        Ok(())
    }

    fn emit_mapping(mut self, writer: &mut impl std::fmt::Write) -> Result<(), std::fmt::Error> {
        self.emit_type_tag(writer, false)?;

        let long_form = self.seq.items.iter().any(|item| {
            item.key_node.is_some_and(|key_node| {
                let key = &self.doc.nodes[key_node];
                key.is_long_form_item()
            }) || self.doc.nodes[item.value_node].is_long_form_item()
        }) && self.seq.items.len() > 1;

        writer.write_char('{')?;
        if long_form {
            write_newline(writer, self.indent + 1)?;
            LargeMappingBodyEmitter {
                state: self.state,
                doc: self.doc,
                seq: self.seq,
                indent: self.indent + 1,
                need_newlines: 0,
                did_emit_items: false,
            }
            .emit(writer)?;
            write_newline(writer, self.indent)?;
        } else {
            if !self.seq.items.is_empty() {
                writer.write_char(' ')?;
            }
            InlineMappingBodyEmitter {
                state: self.state,
                doc: self.doc,
                seq: self.seq,
                indent: self.indent,
            }
            .emit(writer)?;
            if !self.seq.items.is_empty() {
                writer.write_char(' ')?;
            }
        }
        writer.write_char('}')
    }

    fn emit_list(mut self, writer: &mut impl std::fmt::Write) -> Result<(), std::fmt::Error> {
        self.emit_type_tag(writer, false)?;
        let long_form = self
            .seq
            .items
            .iter()
            .any(|item| self.doc.nodes[item.value_node].is_long_form_item())
            && self.seq.items.len() > 1;

        writer.write_char('[')?;
        if long_form {
            write_newline(writer, self.indent + 1)?;
            LargeSequenceBodyEmitter {
                state: self.state,
                doc: self.doc,
                seq: self.seq,
                indent: self.indent + 1,
                need_newlines: 0,
            }
            .emit(writer)?;
            write_newline(writer, self.indent)?;
        } else {
            InlineSequenceBodyEmitter {
                state: self.state,
                doc: self.doc,
                seq: self.seq,
                indent: self.indent,
            }
            .emit(writer)?;
        }
        writer.write_char(']')
    }

    fn emit_tuple(mut self, writer: &mut impl std::fmt::Write) -> Result<(), std::fmt::Error> {
        self.emit_type_tag(writer, true)?;
        let long_form = self
            .seq
            .items
            .iter()
            .any(|item| self.doc.nodes[item.value_node].is_long_form_item())
            && self.seq.items.len() > 1;

        writer.write_char('(')?;
        if long_form {
            write_newline(writer, self.indent + 1)?;
            LargeSequenceBodyEmitter {
                state: self.state,
                doc: self.doc,
                seq: self.seq,
                indent: self.indent + 1,
                need_newlines: 0,
            }
            .emit(writer)?;
            write_newline(writer, self.indent)?;
        } else {
            InlineSequenceBodyEmitter {
                state: self.state,
                doc: self.doc,
                seq: self.seq,
                indent: self.indent,
            }
            .emit(writer)?;
        }
        writer.write_char(')')
    }
}

struct ScalarEmitter<'a> {
    state: &'a mut State,
    doc: &'a Document,
    node: NodeId,
    indent: usize,
}

impl<'a> ScalarEmitter<'a> {
    fn emit(self, writer: &mut impl std::fmt::Write) -> Result<(), std::fmt::Error> {
        let node = &self.doc.nodes[self.node];

        // If the node has an anchor, register the anchor if this is the first
        // time we see it, otherwise emit a reference to the previous node.
        if let Some(anchor) = node.anchor.as_deref() {
            if let Some(reference) = self.state.get_anchor(self.node) {
                writer.write_char('&')?;
                writer.write_str(reference)?;
                return Ok(());
            }

            self.state.set_anchor(self.node, anchor.clone());
            write!(writer, "@{anchor} ")?;
        }

        let scalar = match &node.data {
            NodeData::Scalar(scalar) => scalar.borrow(),
            _ => unreachable!("expected scalar node"),
        };

        let required_style = ScalarStyle::infer(scalar.value);
        let chosen_style = scalar.style.max(required_style);

        match chosen_style {
            ScalarStyle::Plain => writer.write_str(scalar.value),
            ScalarStyle::Quoted => write_dquoted_scalar(scalar.value, writer),
            ScalarStyle::SingleQuoted => write_squoted_scalar(scalar.value, writer),
            ScalarStyle::QuotedTrimWhitespace => {
                write_single_line_block_scalar(scalar.value, writer, self.indent)
            }
            ScalarStyle::QuotedTrimIndent => {
                write_multiline_block_scalar(scalar.value, writer, self.indent)
            }
        }
    }
}

fn escape_char(ch: char, writer: &mut impl std::fmt::Write) -> Result<(), std::fmt::Error> {
    match ch {
        '\x00' => writer.write_str("\\0"),
        '\x07' => writer.write_str("\\a"),
        '\x08' => writer.write_str("\\b"),
        '\x09' => writer.write_str("\\t"),
        '\x0a' => writer.write_str("\\n"),
        '\x0b' => writer.write_str("\\v"),
        '\x0c' => writer.write_str("\\f"),
        '\x0d' => writer.write_str("\\r"),
        '\\' => writer.write_str("\\\\"),
        ch => write!(writer, "\\u{:04x}", ch as u32),
    }
}

fn write_dquoted_scalar(
    value: &str,
    writer: &mut impl std::fmt::Write,
) -> Result<(), std::fmt::Error> {
    writer.write_char('"')?;
    for ch in value.chars() {
        match ch {
            '"' => writer.write_str("\\\"")?,
            ch if ch.must_escape_dquote() => escape_char(ch, writer)?,
            ch => writer.write_char(ch)?,
        }
    }
    writer.write_char('"')
}

fn write_squoted_scalar(
    value: &str,
    writer: &mut impl std::fmt::Write,
) -> Result<(), std::fmt::Error> {
    writer.write_char('\'')?;
    for ch in value.chars() {
        match ch {
            '\'' => writer.write_str("\\\'")?,
            ch if ch.must_escape_squote() => escape_char(ch, writer)?,
            ch => writer.write_char(ch)?,
        }
    }
    writer.write_char('\'')
}

fn write_single_line_block_scalar(
    value: &str,
    writer: &mut impl std::fmt::Write,
    indent: usize,
) -> Result<(), std::fmt::Error> {
    writer.write_str("-\"")?;
    write_newline(writer, indent + 1)?;
    for ch in value.chars() {
        match ch {
            '"' => writer.write_str("\\\"")?,
            ch if ch.must_escape_dquote() => escape_char(ch, writer)?,
            ch => writer.write_char(ch)?,
        }
    }
    write_newline(writer, indent)?;
    writer.write_char('"')
}

fn write_multiline_block_scalar(
    value: &str,
    writer: &mut impl std::fmt::Write,
    indent: usize,
) -> Result<(), std::fmt::Error> {
    writer.write_str("|\"")?;
    for line in value.lines() {
        write_newline(writer, indent + 1)?;
        for ch in line.chars() {
            match ch {
                '"' => writer.write_str("\\\"")?,
                ch if ch.must_escape_dquote() => escape_char(ch, writer)?,
                ch => writer.write_char(ch)?,
            }
        }
    }
    write_newline(writer, indent)?;
    writer.write_char('"')
}

struct LargeMappingBodyEmitter<'a> {
    state: &'a mut State,
    doc: &'a Document,
    seq: &'a SequenceNode,
    indent: usize,
    need_newlines: usize,
    did_emit_items: bool,
}

impl<'a> LargeMappingBodyEmitter<'a> {
    fn emit(mut self, writer: &mut impl std::fmt::Write) -> Result<(), std::fmt::Error> {
        self.did_emit_items = !self.seq.items.is_empty();
        for i in 0..self.seq.items.len() {
            self.write_pending_newlines(writer)?;

            let SequenceItem {
                key_node,
                value_node,
            } = self.seq.items[i];

            if let Some(key_node) = key_node {
                self.write_key(key_node, writer)?;
                self.write_pending_newlines(writer)?;
            }

            self.write_value(value_node, writer)?;
        }
        Ok(())
    }

    fn write_pending_newlines(
        &mut self,
        writer: &mut impl std::fmt::Write,
    ) -> Result<(), std::fmt::Error> {
        while self.need_newlines > 0 {
            write_newline(writer, self.indent)?;
            self.need_newlines -= 1;
        }
        Ok(())
    }

    fn write_key(
        &mut self,
        key_node: NodeId,
        writer: &mut impl std::fmt::Write,
    ) -> Result<(), std::fmt::Error> {
        let reference = self.state.get_anchor(key_node);
        if let Some(reference) = reference {
            // The key has already been emitted with an anchor, so create a reference to it.
            writer.write_char('&')?;
            writer.write_str(reference)?;
            return writer.write_str(": ");
        }

        let node = &self.doc.nodes[key_node];

        if node.is_complex_key() {
            // Complex key sigil.
            writer.write_str("? ")?;
            ValueEmitter {
                state: self.state,
                doc: self.doc,
                node: key_node,
                indent: self.indent,
            }
            .emit(writer)?;
            // Always add an extra newline after a complex key-value pair.
            self.need_newlines += 1;
            return writer.write_str(": ");
        }

        if let Node {
            data: NodeData::Scalar(_),
            anchor: None,
            ..
        } = node
        {
            ScalarEmitter {
                state: self.state,
                doc: self.doc,
                node: key_node,
                indent: self.indent,
            }
            .emit(writer)?;
            writer.write_str(": ")
        } else {
            unreachable!("simple key but not a scalar somehow?!")
        }
    }

    fn write_value(
        &mut self,
        value_node: NodeId,
        writer: &mut impl std::fmt::Write,
    ) -> Result<(), std::fmt::Error> {
        self.need_newlines += 1;
        ValueEmitter {
            state: self.state,
            doc: self.doc,
            node: value_node,
            indent: self.indent,
        }
        .emit(writer)
    }
}

struct LargeSequenceBodyEmitter<'a> {
    state: &'a mut State,
    doc: &'a Document,
    seq: &'a SequenceNode,
    indent: usize,
    need_newlines: usize,
}

impl<'a> LargeSequenceBodyEmitter<'a> {
    fn emit(mut self, writer: &mut impl std::fmt::Write) -> Result<(), std::fmt::Error> {
        for i in 0..self.seq.items.len() {
            self.write_pending_newlines(writer)?;
            let SequenceItem { value_node, .. } = self.seq.items[i];
            self.write_value(value_node, writer)?;
            writer.write_char(',')?;
        }
        Ok(())
    }

    fn write_pending_newlines(
        &mut self,
        writer: &mut impl std::fmt::Write,
    ) -> Result<(), std::fmt::Error> {
        while self.need_newlines > 0 {
            write_newline(writer, self.indent)?;
            self.need_newlines -= 1;
        }
        Ok(())
    }

    fn write_value(
        &mut self,
        value_node: NodeId,
        writer: &mut impl std::fmt::Write,
    ) -> Result<(), std::fmt::Error> {
        self.need_newlines += 1;
        ValueEmitter {
            state: self.state,
            doc: self.doc,
            node: value_node,
            indent: self.indent,
        }
        .emit(writer)
    }
}

struct InlineMappingBodyEmitter<'a> {
    state: &'a mut State,
    doc: &'a Document,
    seq: &'a SequenceNode,
    indent: usize,
}

impl<'a> InlineMappingBodyEmitter<'a> {
    fn emit(self, writer: &mut impl std::fmt::Write) -> Result<(), std::fmt::Error> {
        for i in 0..self.seq.items.len() {
            if i > 0 {
                writer.write_str(", ")?;
            }

            let SequenceItem {
                key_node,
                value_node,
            } = self.seq.items[i];

            if let Some(key_node) = key_node {
                assert!(!self.doc.nodes[key_node].is_complex_key());
                ValueEmitter {
                    state: self.state,
                    doc: self.doc,
                    node: key_node,
                    indent: self.indent,
                }
                .emit(writer)?;
                writer.write_str(": ")?;
            }

            ValueEmitter {
                state: self.state,
                doc: self.doc,
                node: value_node,
                indent: self.indent,
            }
            .emit(writer)?;
        }
        Ok(())
    }
}

struct InlineSequenceBodyEmitter<'a> {
    state: &'a mut State,
    doc: &'a Document,
    seq: &'a SequenceNode,
    indent: usize,
}

impl<'a> InlineSequenceBodyEmitter<'a> {
    fn emit(self, writer: &mut impl std::fmt::Write) -> Result<(), std::fmt::Error> {
        for i in 0..self.seq.items.len() {
            if i > 0 {
                writer.write_str(", ")?;
            }

            let SequenceItem { value_node, .. } = self.seq.items[i];
            ValueEmitter {
                state: self.state,
                doc: self.doc,
                node: value_node,
                indent: self.indent,
            }
            .emit(writer)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{BuildSeq, Builder, MappingBuilder};

    #[test]
    fn basic() -> Result<(), Box<dyn std::error::Error>> {
        let doc = Builder::default().entry("hello", "world")?.build()?;
        let mut result = String::new();
        let emitter = Emitter::new(&mut result);
        emitter.emit(&doc)?;
        assert_eq!(result, "hello: world");
        Ok(())
    }

    #[test]
    fn emit_complex() -> Result<(), Box<dyn std::error::Error>> {
        let doc = Builder::default()
            .entry("hello", "world")?
            .entry(
                "short_object",
                (|b: &mut MappingBuilder| {
                    b.entry("key", "value")?.entry("another", "pair")?;
                    Ok(())
                })
                .with_type_tag("Object"),
            )?
            .entry(
                "long object",
                (|b: &mut MappingBuilder| {
                    b.entry("a very long key with quotes", [1, 2, 3, 4])?
                        .entry("multiple_keys", true)?;
                    Ok(())
                })
                .with_type_tag("LongObject"),
            )?
            .entry(
                "tagged_tuple",
                (1.23, "Hello, World!", true).with_type_tag("TaggedTuple"),
            )?
            .build()?;
        let mut result = String::new();
        let emitter = Emitter::new(&mut result);
        emitter.emit(&doc)?;
        assert_eq!(
            result,
            r#"hello: world
short_object: Object { key: value, another: pair }
"long object": LongObject {
  "a very long key with quotes": [1, 2, 3, 4]
  multiple_keys: true
}
tagged_tuple: TaggedTuple(1.23, "Hello, World!", true)"#
        );
        Ok(())
    }
}
