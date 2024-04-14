use std::marker::PhantomData;

use serde::de::SeqAccess;

use super::{LOCATION_TOKEN, SPANNED_TOKEN, SPAN_TOKEN};
use crate::{DeserializationError, SourceLocation, Span, Spanned, SpannedExt, SpannedValue};

impl<T: serde::Serialize> serde::Serialize for Spanned<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.value.serialize(serializer)
    }
}

impl<'de, T: serde::Deserialize<'de>> serde::Deserialize<'de> for Spanned<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct Visitor<T>(PhantomData<T>);

        impl<'de, T: serde::Deserialize<'de>> serde::de::Visitor<'de> for Visitor<T> {
            type Value = Spanned<T>;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(formatter, "a spanned value")
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::SeqAccess<'de>,
            {
                let span = seq
                    .next_element::<Smuggle<Span>>()?
                    .expect("expected span")
                    .0;
                let value = seq.next_element::<T>()?.expect("expected value");
                Ok(value.in_span(span))
            }
        }

        // Hook into special magic in the deserializer to smuggle the span.
        deserializer.deserialize_tuple_struct(SPANNED_TOKEN, 2, Visitor::<T>(PhantomData))
    }
}

pub(super) struct Smuggle<T>(pub T, pub usize);

macro_rules! impl_smuggle {
    ($token:expr, $name:ident { $($field:ident: $field_ty:ty,)*}) => {
        impl<'de> serde::de::Deserializer<'de> for Smuggle<$name> {
            type Error = DeserializationError;

            fn deserialize_tuple_struct<V>(self, _name: &str, _len: usize, visitor: V) -> Result<V::Value, Self::Error>
            where
                V: serde::de::Visitor<'de>,
            {
                visitor.visit_seq(self)
            }

            fn deserialize_any<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
            where
                V: serde::de::Visitor<'de>,
            {
                unreachable!();
            }

            serde::forward_to_deserialize_any!(
                bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
                bytes byte_buf option unit unit_struct newtype_struct tuple
                seq map struct enum identifier ignored_any
            );
        }

        impl<'de> serde::de::Deserialize<'de> for Smuggle<$name> {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: serde::Deserializer<'de>,
            {
                struct Visitor;
                impl<'de> serde::de::Visitor<'de> for Visitor {
                    type Value = Smuggle<$name>;
                    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                        write!(formatter, "a tuple struct; SpannedValue can only be deserialized from SIML")
                    }
                    fn visit_seq<A>(self, mut seq: A) -> Result<Smuggle<$name>, A::Error>
                    where
                        A: serde::de::SeqAccess<'de>,
                    {
                        $(
                            let $field = seq.next_element::<Smuggle<$field_ty>>()?.expect(concat!("wrong deserializer; expected ", stringify!($field)));
                        )*
                        Ok(Smuggle(
                            $name { $($field: $field.0,)* }, 0
                        ))
                    }
                }
                deserializer.deserialize_tuple_struct($token, 2, Visitor)
            }
        }

        impl<'de> serde::de::SeqAccess<'de> for Smuggle<$name> {
            type Error = DeserializationError;

            #[allow(unused_assignments)]
            fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
            where
                T: serde::de::DeserializeSeed<'de>,
            {
                let mut static_count = 0;
                $(
                    if static_count == self.1 {
                        self.1 += 1;
                        return seed.deserialize(Smuggle(self.0.$field, 0)).map(Some);
                    }
                    static_count += 1;
                )*

                Ok(None)
            }
        }
    };
    ($primitive:ty, $method:ident) => {
        impl<'de> serde::de::Deserializer<'de> for Smuggle<$primitive> {
            type Error = DeserializationError;

            fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
            where
                V: serde::de::Visitor<'de>,
            {
                visitor.$method(self.0 as _)
            }

            serde::forward_to_deserialize_any!(
                bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
                bytes byte_buf option unit unit_struct newtype_struct seq tuple
                tuple_struct map struct enum identifier ignored_any
            );
        }
        impl<'de> serde::de::Deserialize<'de> for Smuggle<$primitive> {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: serde::Deserializer<'de>,
            {
                <$primitive>::deserialize(deserializer).map(|value| Smuggle(value, 0))
            }
        }
    };
}

impl_smuggle!(
    SPAN_TOKEN,
    Span {
        start: SourceLocation,
        end: SourceLocation,
    }
);
impl_smuggle!(
    LOCATION_TOKEN,
    SourceLocation {
        line: usize,
        column: usize,
        offset: usize,
    }
);
impl_smuggle!(usize, visit_u64);

impl<'a, 'de: 'a> SeqAccess<'de> for Smuggle<&'a SpannedValue<'de>> {
    type Error = DeserializationError;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: serde::de::DeserializeSeed<'de>,
    {
        let deserialized = match self.1 {
            0 => seed.deserialize(Smuggle(self.0.span(), 0)).map(Some),
            1 => seed.deserialize(self.0).map(Some),
            _ => return Ok(None),
        };
        self.1 += 1;
        deserialized
    }
}

const SOURCE_LOCATION_LINE: &str = "line";
const SOURCE_LOCATION_COLUMN: &str = "column";
const SOURCE_LOCATION_OFFSET: &str = "offset";

impl serde::ser::Serialize for SourceLocation {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeStruct;
        let mut state = serializer.serialize_struct("SourceLocation", 3)?;
        state.serialize_field(SOURCE_LOCATION_LINE, &self.line)?;
        state.serialize_field(SOURCE_LOCATION_COLUMN, &self.column)?;
        state.serialize_field(SOURCE_LOCATION_OFFSET, &self.offset)?;
        state.end()
    }
}

impl<'de> serde::de::Deserialize<'de> for SourceLocation {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct Visitor;
        impl<'de> serde::de::Visitor<'de> for Visitor {
            type Value = SourceLocation;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(formatter, "a source location")
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::MapAccess<'de>,
            {
                let mut line = None;
                let mut column = None;
                let mut offset = None;
                while let Some(key) = map.next_key::<&str>()? {
                    match key {
                        SOURCE_LOCATION_LINE => {
                            line = Some(map.next_value()?);
                        }
                        SOURCE_LOCATION_COLUMN => {
                            column = Some(map.next_value()?);
                        }
                        SOURCE_LOCATION_OFFSET => {
                            offset = Some(map.next_value()?);
                        }
                        _ => {
                            return Err(serde::de::Error::unknown_field(
                                key,
                                &[
                                    SOURCE_LOCATION_LINE,
                                    SOURCE_LOCATION_COLUMN,
                                    SOURCE_LOCATION_OFFSET,
                                ],
                            ))
                        }
                    }
                }

                match (line, column, offset) {
                    (Some(line), Some(column), Some(offset)) => Ok(SourceLocation {
                        line,
                        column,
                        offset,
                    }),
                    _ => Err(serde::de::Error::custom(
                        "expected line, column, and offset fields",
                    )),
                }
            }
        }

        deserializer.deserialize_struct(
            "SourceLocation",
            &[
                SOURCE_LOCATION_LINE,
                SOURCE_LOCATION_COLUMN,
                SOURCE_LOCATION_OFFSET,
            ],
            Visitor,
        )
    }
}

const SPAN_START: &str = "start";
const SPAN_END: &str = "end";

impl serde::ser::Serialize for Span {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::ser::Serializer,
    {
        use serde::ser::SerializeStruct;
        let mut state = serializer.serialize_struct("Span", 2)?;
        state.serialize_field(SPAN_START, &self.start)?;
        state.serialize_field(SPAN_END, &self.end)?;
        state.end()
    }
}

impl<'de> serde::de::Deserialize<'de> for Span {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct SpanVisitor;

        impl<'de> serde::de::Visitor<'de> for SpanVisitor {
            type Value = Span;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(formatter, "a span")
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::MapAccess<'de>,
            {
                enum SpanField {
                    Start,
                    End,
                }

                impl<'de> serde::Deserialize<'de> for SpanField {
                    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
                    where
                        D: serde::Deserializer<'de>,
                    {
                        struct Visitor;
                        impl<'de> serde::de::Visitor<'de> for Visitor {
                            type Value = SpanField;

                            #[inline]
                            fn expecting(
                                &self,
                                formatter: &mut std::fmt::Formatter,
                            ) -> std::fmt::Result {
                                write!(formatter, "`{}` or `{}`", SPAN_START, SPAN_END)
                            }

                            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
                            where
                                E: serde::de::Error,
                            {
                                Ok(match v {
                                    SPAN_START => SpanField::Start,
                                    SPAN_END => SpanField::End,
                                    _ => {
                                        return Err(serde::de::Error::unknown_field(
                                            v,
                                            &[SPAN_START, SPAN_END],
                                        ))
                                    }
                                })
                            }
                        }

                        deserializer.deserialize_str(Visitor)
                    }
                }

                let mut start = None;
                let mut end = None;
                for _ in 0..2 {
                    let key = map.next_key::<SpanField>()?;
                    match key {
                        Some(SpanField::Start) => {
                            start = Some(map.next_value()?);
                        }
                        Some(SpanField::End) => {
                            end = Some(map.next_value()?);
                        }
                        None => break,
                    }
                }

                match (start, end) {
                    (Some(start), Some(end)) => Ok(Span { start, end }),
                    _ => Err(serde::de::Error::custom("expected start and end fields")),
                }
            }
        }

        deserializer.deserialize_struct("Span", &[SPAN_START, SPAN_END], SpanVisitor)
    }
}

#[cfg(test)]
mod tests {
    use serde::Deserialize;

    use crate::Scalar;

    use super::*;

    #[test]
    fn smuggle_span() {
        let span = Span {
            start: SourceLocation {
                line: 1,
                column: 2,
                offset: 3,
            },
            end: SourceLocation {
                line: 4,
                column: 5,
                offset: 6,
            },
        };
        let spanned_value = SpannedValue::Scalar(Scalar::plain("hello").in_span(span));

        let deserialized: Spanned<&str> = Spanned::deserialize(&spanned_value).unwrap();
        assert_eq!(deserialized.value, "hello");
        assert_eq!(deserialized.span, span);
    }
}
