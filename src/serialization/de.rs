use serde::de::{
    value::{StrDeserializer, UnitDeserializer},
    Unexpected,
};

use crate::{
    Scalar, ScalarStyle, SequenceStyle, SourceLocation, Spanned, SpannedSequence, SpannedValue,
};

use super::{Smuggle, LOCATION_TOKEN, SPANNED_TOKEN, SPAN_TOKEN};

impl<'a> From<&'a SpannedSequence<'_>> for Unexpected<'a> {
    #[inline]
    fn from(value: &SpannedSequence) -> Self {
        if let Some(_type_tag) = value.type_tag.as_ref() {
            match value.style {
                SequenceStyle::Mapping => Unexpected::StructVariant,
                SequenceStyle::List | SequenceStyle::Tuple => Unexpected::TupleVariant,
            }
        } else {
            match value.style {
                SequenceStyle::Mapping => Unexpected::Map,
                SequenceStyle::List | SequenceStyle::Tuple => Unexpected::Seq,
            }
        }
    }
}

impl<'a> From<&'a SpannedValue<'_>> for Unexpected<'a> {
    #[inline]
    fn from(value: &'a SpannedValue<'_>) -> Self {
        match value {
            SpannedValue::Scalar(Spanned {
                value: Scalar { value, .. },
                ..
            }) => Unexpected::Str(value),
            SpannedValue::Sequence(seq) => seq.into(),
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum DeserializationError {
    #[error("{0}")]
    Custom(String),
    #[error("failed parsing integer: {0} at {1}")]
    ParseInt(std::num::ParseIntError, SourceLocation),
    #[error("failed parsing float: {0} at {1}")]
    ParseFloat(std::num::ParseFloatError, SourceLocation),
    #[error(transparent)]
    ParseDocument(#[from] crate::Error),
}

impl serde::de::Error for DeserializationError {
    fn custom<T: std::fmt::Display>(msg: T) -> Self {
        Self::Custom(msg.to_string())
    }
}

fn expect_scalar<'a, 'de>(
    value: &'a SpannedValue<'de>,
) -> Result<Spanned<Scalar<'de>>, DeserializationError> {
    match value {
        SpannedValue::Scalar(scalar) => Ok(*scalar),
        SpannedValue::Sequence(_) => Err(serde::de::Error::invalid_type(value.into(), &"scalar")),
    }
}

impl<'de> serde::Deserializer<'de> for Spanned<Scalar<'de>> {
    type Error = DeserializationError;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        // Quoted scalars are always strings.
        if self.value.style != ScalarStyle::Plain {
            return visitor.visit_borrowed_str(self.value.value);
        }

        // If the scalar is plain, try to parse it with some common formats.
        // Otherwise just pass it as a string.
        if self.value.value == "true" {
            visitor.visit_bool(true)
        } else if self.value.value == "false" {
            visitor.visit_bool(false)
        } else if let Ok(value) = self.parse::<u64>() {
            visitor.visit_u64(value)
        } else if let Ok(value) = self.parse::<i64>() {
            visitor.visit_i64(value)
        } else if let Ok(value) = self.parse::<f64>() {
            visitor.visit_f64(value)
        } else {
            visitor.visit_borrowed_str(self.value.value)
        }
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.value.value {
            "true" => visitor.visit_bool(true),
            "false" => visitor.visit_bool(false),
            value => Err(serde::de::Error::invalid_value(
                Unexpected::Str(value),
                &"true or false",
            )),
        }
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.parse() {
            Ok(value) => visitor.visit_i8(value),
            Err(err) => Err(DeserializationError::ParseInt(err, self.span.start)),
        }
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.parse() {
            Ok(value) => visitor.visit_i16(value),
            Err(err) => Err(DeserializationError::ParseInt(err, self.span.start)),
        }
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.parse() {
            Ok(value) => visitor.visit_i32(value),
            Err(err) => Err(DeserializationError::ParseInt(err, self.span.start)),
        }
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.parse() {
            Ok(value) => visitor.visit_i64(value),
            Err(err) => Err(DeserializationError::ParseInt(err, self.span.start)),
        }
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.parse() {
            Ok(value) => visitor.visit_u8(value),
            Err(err) => Err(DeserializationError::ParseInt(err, self.span.start)),
        }
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.parse() {
            Ok(value) => visitor.visit_u16(value),
            Err(err) => Err(DeserializationError::ParseInt(err, self.span.start)),
        }
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.parse() {
            Ok(value) => visitor.visit_u32(value),
            Err(err) => Err(DeserializationError::ParseInt(err, self.span.start)),
        }
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.parse() {
            Ok(value) => visitor.visit_u64(value),
            Err(err) => Err(DeserializationError::ParseInt(err, self.span.start)),
        }
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.parse() {
            Ok(value) => visitor.visit_f32(value),
            Err(err) => Err(DeserializationError::ParseFloat(err, self.span.start)),
        }
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.parse() {
            Ok(value) => visitor.visit_f64(value),
            Err(err) => Err(DeserializationError::ParseFloat(err, self.span.start)),
        }
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        if self.len() == 1 {
            visitor.visit_char(self.chars().next().unwrap())
        } else {
            Err(serde::de::Error::invalid_value(
                Unexpected::Str(&*self),
                &"a single character",
            ))
        }
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_borrowed_str(self.value.value)
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_borrowed_str(self.value.value)
    }

    fn deserialize_bytes<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        todo!("base64 decoding")
    }

    fn deserialize_byte_buf<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        todo!("base64 decoding")
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        if self.value.value == "None" {
            visitor.visit_none()
        } else {
            Err(serde::de::Error::invalid_type(
                Unexpected::Str(self.value.value),
                &"a tuple-style sequence with the tag `Some`",
            ))
        }
    }

    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        Err(serde::de::Error::invalid_type(
            Unexpected::Str(self.value.value),
            &visitor,
        ))
    }

    fn deserialize_unit_struct<V>(
        self,
        name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        if self.value.value == name {
            return visitor.visit_unit();
        }
        Err(serde::de::Error::invalid_type(
            Unexpected::Str(self.value.value),
            &visitor,
        ))
    }

    fn deserialize_newtype_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        Err(serde::de::Error::invalid_type(
            Unexpected::Str(self.value.value),
            &visitor,
        ))
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        Err(serde::de::Error::invalid_type(
            Unexpected::Str(self.value.value),
            &visitor,
        ))
    }

    fn deserialize_tuple<V>(self, _len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        Err(serde::de::Error::invalid_type(
            Unexpected::Str(self.value.value),
            &visitor,
        ))
    }

    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        Err(serde::de::Error::invalid_type(
            Unexpected::Str(self.value.value),
            &visitor,
        ))
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        Err(serde::de::Error::invalid_type(
            Unexpected::Str(self.value.value),
            &visitor,
        ))
    }

    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        Err(serde::de::Error::invalid_type(
            Unexpected::Str(self.value.value),
            &visitor,
        ))
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_enum(StrDeserializer::new(self.value.value))
    }

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_borrowed_str(self.value.value)
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_borrowed_str(self.value.value)
    }
}

impl<'a, 'de> serde::Deserializer<'de> for &'a SpannedValue<'de>
where
    'de: 'a,
{
    type Error = DeserializationError;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self {
            SpannedValue::Scalar(scalar) => scalar.deserialize_any(visitor),
            SpannedValue::Sequence(seq) => {
                if let Some(type_tag) = seq.type_tag {
                    visitor.visit_enum(EnumAccessDataCarrying(seq, type_tag))
                } else {
                    match seq.style {
                        SequenceStyle::Mapping => visitor.visit_map(MapAccess {
                            seq,
                            index: 0,
                            next_is_value: false,
                        }),
                        SequenceStyle::List | SequenceStyle::Tuple => {
                            visitor.visit_seq(SeqAccess { seq, index: 0 })
                        }
                    }
                }
            }
        }
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        expect_scalar(self)?.deserialize_bool(visitor)
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        expect_scalar(self)?.deserialize_i8(visitor)
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        expect_scalar(self)?.deserialize_i16(visitor)
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        expect_scalar(self)?.deserialize_i32(visitor)
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        expect_scalar(self)?.deserialize_i64(visitor)
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        expect_scalar(self)?.deserialize_u8(visitor)
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        expect_scalar(self)?.deserialize_u16(visitor)
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        expect_scalar(self)?.deserialize_u32(visitor)
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        expect_scalar(self)?.deserialize_u64(visitor)
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        expect_scalar(self)?.deserialize_f32(visitor)
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        expect_scalar(self)?.deserialize_f64(visitor)
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        expect_scalar(self)?.deserialize_char(visitor)
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        expect_scalar(self)?.deserialize_str(visitor)
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        expect_scalar(self)?.deserialize_string(visitor)
    }

    fn deserialize_bytes<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        todo!("base64-decoding?")
    }

    fn deserialize_byte_buf<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        todo!("base64-decoding?")
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self {
            SpannedValue::Scalar(scalar) => scalar.deserialize_option(visitor),
            SpannedValue::Sequence(seq) => {
                if seq.style == SequenceStyle::Tuple
                    && seq.items.len() == 1
                    && seq.type_tag.as_ref().map(|t| t.value.value) == Some("Some")
                {
                    visitor.visit_some(&seq.items[0].1)
                } else {
                    Err(serde::de::Error::invalid_type(self.into(), &visitor))
                }
            }
        }
    }

    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        if let SpannedValue::Sequence(seq) = self {
            if seq.style == SequenceStyle::Tuple && seq.items.is_empty() {
                return visitor.visit_unit();
            }
        }
        Err(serde::de::Error::invalid_type(self.into(), &visitor))
    }

    fn deserialize_unit_struct<V>(
        self,
        name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self {
            SpannedValue::Scalar(scalar) => scalar.deserialize_unit_struct(name, visitor),
            SpannedValue::Sequence(seq) => {
                if seq.type_tag() == Some(name) && seq.items.is_empty() {
                    visitor.visit_unit()
                } else {
                    Err(serde::de::Error::invalid_type(self.into(), &visitor))
                }
            }
        }
    }

    fn deserialize_newtype_struct<V>(
        self,
        name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        if let SpannedValue::Sequence(seq) = self {
            if seq.type_tag() == Some(name) && seq.items.len() == 1 {
                return visitor.visit_newtype_struct(&seq.items[0].1);
            }
        }

        Err(serde::de::Error::invalid_type(self.into(), &visitor))
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        if let SpannedValue::Sequence(seq) = self {
            if seq.style != SequenceStyle::Mapping && seq.type_tag.is_none() {
                return visitor.visit_seq(SeqAccess { seq, index: 0 });
            }
        }

        Err(serde::de::Error::invalid_type(self.into(), &visitor))
    }

    fn deserialize_tuple<V>(self, len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        if let SpannedValue::Sequence(seq) = self {
            if seq.style != SequenceStyle::Mapping && seq.type_tag.is_none() {
                if seq.items.len() == len {
                    return visitor.visit_seq(SeqAccess { seq, index: 0 });
                }
            }
        }

        Err(serde::de::Error::invalid_type(self.into(), &visitor))
    }

    fn deserialize_tuple_struct<V>(
        self,
        name: &'static str,
        len: usize,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        if name == SPANNED_TOKEN {
            return visitor.visit_seq(Smuggle(self, 0));
        } else if name == SPAN_TOKEN {
            return visitor.visit_seq(Smuggle(self, 0));
        } else if name == LOCATION_TOKEN {
            return visitor.visit_seq(Smuggle(self, 0));
        }

        if let SpannedValue::Sequence(seq) = self {
            if seq.type_tag() == Some(name) && seq.items.len() == len {
                return visitor.visit_seq(SeqAccess { seq, index: 0 });
            }
        }

        Err(serde::de::Error::invalid_type(self.into(), &visitor))
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        if let SpannedValue::Sequence(seq) = self {
            if seq.style == SequenceStyle::Mapping {
                return visitor.visit_map(MapAccess {
                    seq,
                    index: 0,
                    next_is_value: false,
                });
            }
        }

        Err(serde::de::Error::invalid_type(self.into(), &visitor))
    }

    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        if let SpannedValue::Sequence(seq) = self {
            return visitor.visit_map(MapAccess {
                seq,
                index: 0,
                next_is_value: false,
            });
        }

        Err(serde::de::Error::invalid_type(self.into(), &visitor))
    }

    fn deserialize_enum<V>(
        self,
        name: &'static str,
        variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self {
            SpannedValue::Scalar(scalar) => scalar.deserialize_enum(name, variants, visitor),
            SpannedValue::Sequence(seq) => {
                if let Some(type_tag) = seq.type_tag {
                    visitor.visit_enum(EnumAccessDataCarrying(seq, type_tag))
                } else {
                    Err(serde::de::Error::invalid_type(self.into(), &visitor))
                }
            }
        }
    }

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        expect_scalar(self)?.deserialize_identifier(visitor)
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_unit()
    }
}

struct EnumAccessDataCarrying<'a, 'de>(&'a SpannedSequence<'de>, Spanned<Scalar<'de>>);
impl<'a, 'de> serde::de::EnumAccess<'de> for EnumAccessDataCarrying<'a, 'de>
where
    'de: 'a,
{
    type Error = DeserializationError;
    type Variant = EnumVariantAccessDataCarrying<'a, 'de>;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant), Self::Error>
    where
        V: serde::de::DeserializeSeed<'de>,
    {
        let type_tag = seed.deserialize(self.1)?;
        Ok((type_tag, EnumVariantAccessDataCarrying(self.0)))
    }
}

struct EnumVariantAccessDataCarrying<'a, 'de>(&'a SpannedSequence<'de>);
impl<'a, 'de> serde::de::VariantAccess<'de> for EnumVariantAccessDataCarrying<'a, 'de>
where
    'de: 'a,
{
    type Error = DeserializationError;

    fn unit_variant(self) -> Result<(), Self::Error> {
        if self.0.items.is_empty() {
            Ok(())
        } else {
            Err(serde::de::Error::invalid_type(
                Unexpected::Seq,
                &"empty sequence",
            ))
        }
    }

    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value, Self::Error>
    where
        T: serde::de::DeserializeSeed<'de>,
    {
        if self.0.items.len() == 1 {
            seed.deserialize(&self.0.items[0].1)
        } else {
            Err(serde::de::Error::invalid_length(self.0.items.len(), &"1"))
        }
    }

    fn tuple_variant<V>(self, _len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_seq(SeqAccess {
            seq: self.0,
            index: 0,
        })
    }

    fn struct_variant<V>(
        self,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_map(MapAccess {
            seq: self.0,
            index: 0,
            next_is_value: false,
        })
    }
}

struct SeqAccess<'a, 'de> {
    seq: &'a SpannedSequence<'de>,
    index: usize,
}
impl<'a, 'de> serde::de::SeqAccess<'de> for SeqAccess<'a, 'de>
where
    'de: 'a,
{
    type Error = DeserializationError;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: serde::de::DeserializeSeed<'de>,
    {
        let index = self.index;
        if index < self.seq.items.len() {
            self.index += 1;
            seed.deserialize(&self.seq.items[index].1).map(Some)
        } else {
            Ok(None)
        }
    }
}

struct MapAccess<'a, 'de> {
    seq: &'a SpannedSequence<'de>,
    index: usize,
    next_is_value: bool,
}
impl<'a, 'de> serde::de::MapAccess<'de> for MapAccess<'a, 'de>
where
    'de: 'a,
{
    type Error = DeserializationError;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: serde::de::DeserializeSeed<'de>,
    {
        if self.next_is_value {
            panic!("cannot call next_key() before a corresponding call to next_value() in the previous iteration");
        }
        if self.index < self.seq.items.len() {
            self.next_is_value = true;
            if let Some(key) = &self.seq.items[self.index].0 {
                seed.deserialize(key).map(Some)
            } else {
                seed.deserialize(UnitDeserializer::new()).map(Some)
            }
        } else {
            Ok(None)
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::DeserializeSeed<'de>,
    {
        if !self.next_is_value {
            panic!("cannot call next_value() before next_key()");
        }
        assert!(
            self.index < self.seq.items.len(),
            "cannot call next_value() after next_key() as returned `None`"
        );
        self.next_is_value = false;
        let index = self.index;
        self.index += 1;
        seed.deserialize(&self.seq.items[index].1)
    }
}
