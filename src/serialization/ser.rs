use crate::{BuilderError, Document, NodeId, Scalar, SequenceStyle};

#[derive(Debug, thiserror::Error)]
pub enum SerializationError {
    #[error(transparent)]
    Builder(#[from] BuilderError),
    #[error("expected a struct or mapping")]
    ExpectedStructOrMapping,
    #[error(transparent)]
    Write(#[from] std::fmt::Error),
    #[error("{0}")]
    Custom(String),
}

impl serde::ser::Error for SerializationError {
    fn custom<T>(msg: T) -> Self
    where
        T: std::fmt::Display,
    {
        Self::Custom(msg.to_string())
    }
}

#[derive(Default)]
pub(super) struct RootSerializer {
    doc: Document,
    pending_key: Option<NodeId>,
    tag_structs: bool,
}

type Impossible = serde::ser::Impossible<RootSerializer, SerializationError>;

impl RootSerializer {
    #[inline]
    pub fn finish(self) -> Result<Document, SerializationError> {
        self.doc.check_complete()?;
        Ok(self.doc)
    }
}

impl serde::ser::Serializer for RootSerializer {
    type Ok = Self;
    type Error = SerializationError;
    type SerializeSeq = Self;
    type SerializeTuple = Impossible;
    type SerializeTupleStruct = Impossible;
    type SerializeTupleVariant = Impossible;
    type SerializeMap = Self;
    type SerializeStruct = Self;
    type SerializeStructVariant = Impossible;

    fn serialize_bool(self, _v: bool) -> Result<Self::Ok, Self::Error> {
        Err(SerializationError::ExpectedStructOrMapping)
    }

    fn serialize_i8(self, _v: i8) -> Result<Self::Ok, Self::Error> {
        Err(SerializationError::ExpectedStructOrMapping)
    }

    fn serialize_i16(self, _v: i16) -> Result<Self::Ok, Self::Error> {
        Err(SerializationError::ExpectedStructOrMapping)
    }

    fn serialize_i32(self, _v: i32) -> Result<Self::Ok, Self::Error> {
        Err(SerializationError::ExpectedStructOrMapping)
    }

    fn serialize_i64(self, _v: i64) -> Result<Self::Ok, Self::Error> {
        Err(SerializationError::ExpectedStructOrMapping)
    }

    fn serialize_u8(self, _v: u8) -> Result<Self::Ok, Self::Error> {
        Err(SerializationError::ExpectedStructOrMapping)
    }

    fn serialize_u16(self, _v: u16) -> Result<Self::Ok, Self::Error> {
        Err(SerializationError::ExpectedStructOrMapping)
    }

    fn serialize_u32(self, _v: u32) -> Result<Self::Ok, Self::Error> {
        Err(SerializationError::ExpectedStructOrMapping)
    }

    fn serialize_u64(self, _v: u64) -> Result<Self::Ok, Self::Error> {
        Err(SerializationError::ExpectedStructOrMapping)
    }

    fn serialize_f32(self, _v: f32) -> Result<Self::Ok, Self::Error> {
        Err(SerializationError::ExpectedStructOrMapping)
    }

    fn serialize_f64(self, _v: f64) -> Result<Self::Ok, Self::Error> {
        Err(SerializationError::ExpectedStructOrMapping)
    }

    fn serialize_char(self, _v: char) -> Result<Self::Ok, Self::Error> {
        Err(SerializationError::ExpectedStructOrMapping)
    }

    fn serialize_str(self, _v: &str) -> Result<Self::Ok, Self::Error> {
        Err(SerializationError::ExpectedStructOrMapping)
    }

    fn serialize_bytes(self, _v: &[u8]) -> Result<Self::Ok, Self::Error> {
        Err(SerializationError::ExpectedStructOrMapping)
    }

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        Err(SerializationError::ExpectedStructOrMapping)
    }

    fn serialize_some<T: ?Sized>(self, _value: &T) -> Result<Self::Ok, Self::Error>
    where
        T: serde::Serialize,
    {
        Err(SerializationError::ExpectedStructOrMapping)
    }

    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        Err(SerializationError::ExpectedStructOrMapping)
    }

    fn serialize_unit_struct(self, _name: &'static str) -> Result<Self::Ok, Self::Error> {
        Err(SerializationError::ExpectedStructOrMapping)
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        Err(SerializationError::ExpectedStructOrMapping)
    }

    fn serialize_newtype_struct<T: ?Sized>(
        self,
        _name: &'static str,
        _value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: serde::Serialize,
    {
        Err(SerializationError::ExpectedStructOrMapping)
    }

    fn serialize_newtype_variant<T: ?Sized>(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: serde::Serialize,
    {
        Err(SerializationError::ExpectedStructOrMapping)
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        Ok(self)
    }

    fn serialize_tuple(self, _len: usize) -> Result<Self::SerializeTuple, Self::Error> {
        Err(SerializationError::ExpectedStructOrMapping)
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        Err(SerializationError::ExpectedStructOrMapping)
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        Err(SerializationError::ExpectedStructOrMapping)
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
        Ok(self)
    }

    fn serialize_struct(
        self,
        _name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        Ok(self)
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        Err(SerializationError::ExpectedStructOrMapping)
    }

    fn collect_str<T: ?Sized>(self, value: &T) -> Result<Self::Ok, Self::Error>
    where
        T: std::fmt::Display,
    {
        self.serialize_str(&value.to_string())
    }

    #[inline]
    fn is_human_readable(&self) -> bool {
        true
    }
}

impl serde::ser::SerializeSeq for RootSerializer {
    type Ok = Self;
    type Error = SerializationError;

    fn serialize_element<T: ?Sized>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: serde::Serialize,
    {
        let node = value.serialize(NodeSerializer {
            doc: &mut self.doc,
            tag_structs: self.tag_structs,
        })?;
        self.doc.add_sequence_item(None, None, node);
        Ok(())
    }

    #[inline]
    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(self)
    }
}

impl serde::ser::SerializeMap for RootSerializer {
    type Ok = Self;
    type Error = SerializationError;

    fn serialize_key<T: ?Sized>(&mut self, key: &T) -> Result<(), Self::Error>
    where
        T: serde::Serialize,
    {
        assert!(
            self.pending_key.is_none(),
            "serialize_key() without matching serialize_value()"
        );
        self.pending_key = Some(key.serialize(NodeSerializer {
            doc: &mut self.doc,
            tag_structs: self.tag_structs,
        })?);
        Ok(())
    }

    fn serialize_value<T: ?Sized>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: serde::Serialize,
    {
        let value_node = value.serialize(NodeSerializer {
            doc: &mut self.doc,
            tag_structs: self.tag_structs,
        })?;
        self.doc
            .add_sequence_item(None, self.pending_key.take(), value_node);
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(self)
    }

    fn serialize_entry<K: ?Sized, V: ?Sized>(
        &mut self,
        key: &K,
        value: &V,
    ) -> Result<(), Self::Error>
    where
        K: serde::Serialize,
        V: serde::Serialize,
    {
        let key_node = key.serialize(NodeSerializer {
            doc: &mut self.doc,
            tag_structs: self.tag_structs,
        })?;
        let value_node = value.serialize(NodeSerializer {
            doc: &mut self.doc,
            tag_structs: self.tag_structs,
        })?;
        self.doc.add_sequence_item(None, Some(key_node), value_node);
        Ok(())
    }
}

impl serde::ser::SerializeStruct for RootSerializer {
    type Ok = Self;
    type Error = SerializationError;

    fn serialize_field<T: ?Sized>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), Self::Error>
    where
        T: serde::Serialize,
    {
        let key_node = self.doc.add_scalar(Scalar::plain(key))?;
        let value_node = value.serialize(NodeSerializer {
            doc: &mut self.doc,
            tag_structs: self.tag_structs,
        })?;
        self.doc.add_sequence_item(None, Some(key_node), value_node);
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(self)
    }
}

struct NodeSerializer<'a> {
    doc: &'a mut Document,
    tag_structs: bool,
}
impl<'a> serde::ser::Serializer for NodeSerializer<'a> {
    type Ok = NodeId;
    type Error = SerializationError;
    type SerializeSeq = SerializeSeq<'a>;
    type SerializeTuple = SerializeSeq<'a>;
    type SerializeTupleStruct = SerializeSeq<'a>;
    type SerializeTupleVariant = SerializeSeq<'a>;
    type SerializeMap = SerializeSeq<'a>;
    type SerializeStruct = SerializeSeq<'a>;
    type SerializeStructVariant = SerializeSeq<'a>;

    fn serialize_bool(self, v: bool) -> Result<Self::Ok, Self::Error> {
        self.doc
            .add_scalar(Scalar::plain(if v { "true" } else { "false" }))
            .map_err(Into::into)
    }

    fn serialize_i8(self, v: i8) -> Result<Self::Ok, Self::Error> {
        self.doc
            .add_scalar(Scalar::plain(itoa::Buffer::new().format(v)))
            .map_err(Into::into)
    }

    fn serialize_i16(self, v: i16) -> Result<Self::Ok, Self::Error> {
        self.doc
            .add_scalar(Scalar::plain(itoa::Buffer::new().format(v)))
            .map_err(Into::into)
    }

    fn serialize_i32(self, v: i32) -> Result<Self::Ok, Self::Error> {
        self.doc
            .add_scalar(Scalar::plain(itoa::Buffer::new().format(v)))
            .map_err(Into::into)
    }

    fn serialize_i64(self, v: i64) -> Result<Self::Ok, Self::Error> {
        self.doc
            .add_scalar(Scalar::plain(itoa::Buffer::new().format(v)))
            .map_err(Into::into)
    }

    fn serialize_u8(self, v: u8) -> Result<Self::Ok, Self::Error> {
        self.doc
            .add_scalar(Scalar::plain(itoa::Buffer::new().format(v)))
            .map_err(Into::into)
    }

    fn serialize_u16(self, v: u16) -> Result<Self::Ok, Self::Error> {
        self.doc
            .add_scalar(Scalar::plain(itoa::Buffer::new().format(v)))
            .map_err(Into::into)
    }

    fn serialize_u32(self, v: u32) -> Result<Self::Ok, Self::Error> {
        self.doc
            .add_scalar(Scalar::plain(itoa::Buffer::new().format(v)))
            .map_err(Into::into)
    }

    fn serialize_u64(self, v: u64) -> Result<Self::Ok, Self::Error> {
        self.doc
            .add_scalar(Scalar::plain(itoa::Buffer::new().format(v)))
            .map_err(Into::into)
    }

    fn serialize_f32(self, v: f32) -> Result<Self::Ok, Self::Error> {
        self.doc
            .add_scalar(Scalar::plain(ryu::Buffer::new().format(v)))
            .map_err(Into::into)
    }

    fn serialize_f64(self, v: f64) -> Result<Self::Ok, Self::Error> {
        self.doc
            .add_scalar(Scalar::plain(ryu::Buffer::new().format(v)))
            .map_err(Into::into)
    }

    fn serialize_char(self, v: char) -> Result<Self::Ok, Self::Error> {
        let mut buf = [0; 4];
        let s = v.encode_utf8(&mut buf);
        self.doc
            .add_scalar(Scalar::infer_style(s))
            .map_err(Into::into)
    }

    fn serialize_str(self, v: &str) -> Result<Self::Ok, Self::Error> {
        // Note: The emitter will determine the optimal representation for the string.
        self.doc
            .add_scalar(Scalar::infer_style(v))
            .map_err(Into::into)
    }

    fn serialize_bytes(self, _v: &[u8]) -> Result<Self::Ok, Self::Error> {
        todo!("base64 encoding")
    }

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        self.doc
            .add_scalar(Scalar::plain("None"))
            .map_err(Into::into)
    }

    fn serialize_some<T: ?Sized>(self, value: &T) -> Result<Self::Ok, Self::Error>
    where
        T: serde::Serialize,
    {
        self.serialize_newtype_struct("Some", value)
    }

    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        let id = self.doc.add_sequence(SequenceStyle::Tuple, None, None)?;
        self.doc.complete_sequence(id);
        Ok(id)
    }

    fn serialize_unit_struct(self, name: &'static str) -> Result<Self::Ok, Self::Error> {
        self.doc.add_scalar(Scalar::plain(name)).map_err(Into::into)
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        self.doc
            .add_scalar(Scalar::plain(variant))
            .map_err(Into::into)
    }

    fn serialize_newtype_struct<T: ?Sized>(
        self,
        name: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: serde::Serialize,
    {
        // TODO: Should type tags be included here?
        let type_tag = self.doc.add_scalar(Scalar::plain(name))?;
        let seq = self
            .doc
            .add_sequence(SequenceStyle::Tuple, None, Some(type_tag))?;
        let value = value.serialize(NodeSerializer {
            doc: self.doc,
            tag_structs: self.tag_structs,
        })?;
        self.doc.add_sequence_item(Some(seq), None, value);
        self.doc.complete_sequence(seq);
        Ok(seq)
    }

    fn serialize_newtype_variant<T: ?Sized>(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: serde::Serialize,
    {
        self.serialize_newtype_struct(variant, value)
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        let seq = self.doc.add_sequence(SequenceStyle::List, None, None)?;
        Ok(SerializeSeq {
            node: seq,
            doc: self.doc,
            pending_key: None,
            tag_structs: self.tag_structs,
        })
    }

    fn serialize_tuple(self, _len: usize) -> Result<Self::SerializeTuple, Self::Error> {
        let seq = self.doc.add_sequence(SequenceStyle::Tuple, None, None)?;
        Ok(SerializeSeq {
            node: seq,
            doc: self.doc,
            pending_key: None,
            tag_structs: self.tag_structs,
        })
    }

    fn serialize_tuple_struct(
        self,
        name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        // TODO: Should type tags be included?
        let type_tag = self.doc.add_scalar(Scalar::plain(name))?;
        let seq = self
            .doc
            .add_sequence(SequenceStyle::Tuple, None, Some(type_tag))?;
        Ok(SerializeSeq {
            node: seq,
            doc: self.doc,
            pending_key: None,
            tag_structs: self.tag_structs,
        })
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        self.serialize_tuple_struct(variant, len)
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
        let map = self.doc.add_sequence(SequenceStyle::Mapping, None, None)?;
        Ok(SerializeSeq {
            node: map,
            doc: self.doc,
            pending_key: None,
            tag_structs: self.tag_structs,
        })
    }

    fn serialize_struct(
        self,
        name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        let type_tag = if self.tag_structs {
            Some(self.doc.add_scalar(Scalar::plain(name))?)
        } else {
            None
        };

        let map = self
            .doc
            .add_sequence(SequenceStyle::Mapping, None, type_tag)?;
        Ok(SerializeSeq {
            node: map,
            doc: self.doc,
            pending_key: None,
            tag_structs: self.tag_structs,
        })
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        self.serialize_struct(variant, len)
    }

    fn serialize_i128(self, v: i128) -> Result<Self::Ok, Self::Error> {
        self.doc
            .add_scalar(Scalar::plain(itoa::Buffer::new().format(v)))
            .map_err(Into::into)
    }

    fn serialize_u128(self, v: u128) -> Result<Self::Ok, Self::Error> {
        self.doc
            .add_scalar(Scalar::plain(itoa::Buffer::new().format(v)))
            .map_err(Into::into)
    }

    fn is_human_readable(&self) -> bool {
        true
    }

    fn collect_str<T: ?Sized>(self, value: &T) -> Result<Self::Ok, Self::Error>
    where
        T: std::fmt::Display,
    {
        self.serialize_str(&value.to_string())
    }
}

struct SerializeSeq<'a> {
    node: NodeId,
    doc: &'a mut Document,
    pending_key: Option<NodeId>,
    tag_structs: bool,
}

impl<'a> serde::ser::SerializeSeq for SerializeSeq<'a> {
    type Ok = NodeId;
    type Error = SerializationError;

    fn serialize_element<T: ?Sized>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: serde::Serialize,
    {
        let element = value.serialize(NodeSerializer {
            doc: self.doc,
            tag_structs: self.tag_structs,
        })?;
        self.doc.add_sequence_item(Some(self.node), None, element);
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.doc.complete_sequence(self.node);
        Ok(self.node)
    }
}

impl<'a> serde::ser::SerializeMap for SerializeSeq<'a> {
    type Ok = NodeId;
    type Error = SerializationError;

    fn serialize_key<T: ?Sized>(&mut self, key: &T) -> Result<(), Self::Error>
    where
        T: serde::Serialize,
    {
        assert!(
            self.pending_key.is_none(),
            "serialize_key() without matching serialize_value()"
        );
        self.pending_key = Some(key.serialize(NodeSerializer {
            doc: self.doc,
            tag_structs: self.tag_structs,
        })?);
        Ok(())
    }

    fn serialize_value<T: ?Sized>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: serde::Serialize,
    {
        let value_node = value.serialize(NodeSerializer {
            doc: self.doc,
            tag_structs: self.tag_structs,
        })?;
        self.doc
            .add_sequence_item(Some(self.node), self.pending_key.take(), value_node);
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.doc.complete_sequence(self.node);
        Ok(self.node)
    }

    fn serialize_entry<K: ?Sized, V: ?Sized>(
        &mut self,
        key: &K,
        value: &V,
    ) -> Result<(), Self::Error>
    where
        K: serde::Serialize,
        V: serde::Serialize,
    {
        assert!(
            self.pending_key.is_none(),
            "mixed serialize_entry() with unpaired serialize_key()"
        );
        let key_node = key.serialize(NodeSerializer {
            doc: self.doc,
            tag_structs: self.tag_structs,
        })?;
        let value_node = value.serialize(NodeSerializer {
            doc: self.doc,
            tag_structs: self.tag_structs,
        })?;
        self.doc
            .add_sequence_item(Some(self.node), Some(key_node), value_node);
        Ok(())
    }
}

impl<'a> serde::ser::SerializeTuple for SerializeSeq<'a> {
    type Ok = NodeId;
    type Error = SerializationError;

    fn serialize_element<T: ?Sized>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: serde::Serialize,
    {
        <Self as serde::ser::SerializeSeq>::serialize_element(self, value)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.doc.complete_sequence(self.node);
        Ok(self.node)
    }
}

impl<'a> serde::ser::SerializeTupleStruct for SerializeSeq<'a> {
    type Ok = NodeId;
    type Error = SerializationError;

    fn serialize_field<T: ?Sized>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: serde::Serialize,
    {
        <Self as serde::ser::SerializeSeq>::serialize_element(self, value)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.doc.complete_sequence(self.node);
        Ok(self.node)
    }
}

impl<'a> serde::ser::SerializeTupleVariant for SerializeSeq<'a> {
    type Ok = NodeId;
    type Error = SerializationError;

    fn serialize_field<T: ?Sized>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: serde::Serialize,
    {
        <Self as serde::ser::SerializeSeq>::serialize_element(self, value)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.doc.complete_sequence(self.node);
        Ok(self.node)
    }
}

impl<'a> serde::ser::SerializeStruct for SerializeSeq<'a> {
    type Ok = NodeId;
    type Error = SerializationError;

    fn serialize_field<T: ?Sized>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), Self::Error>
    where
        T: serde::Serialize,
    {
        let key_node = self.doc.add_scalar(Scalar::plain(key))?;
        let value_node = value.serialize(NodeSerializer {
            doc: self.doc,
            tag_structs: self.tag_structs,
        })?;
        self.doc
            .add_sequence_item(Some(self.node), Some(key_node), value_node);
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.doc.complete_sequence(self.node);
        Ok(self.node)
    }
}

impl<'a> serde::ser::SerializeStructVariant for SerializeSeq<'a> {
    type Ok = NodeId;
    type Error = SerializationError;

    fn serialize_field<T: ?Sized>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), Self::Error>
    where
        T: serde::Serialize,
    {
        <Self as serde::ser::SerializeStruct>::serialize_field(self, key, value)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.doc.complete_sequence(self.node);
        Ok(self.node)
    }
}
