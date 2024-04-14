#![cfg(feature = "serde")]

mod de;
mod ser;
mod spanned;

use std::fmt::Write;

pub use de::*;
pub use ser::*;
use spanned::*;

use crate::{emitter::Emitter, Document, Error, ParseStream, SpannedValue, Value};

use serde::{Deserialize, Serialize};

/// Name of magical field that can be used during deserialization to get the
/// source span of a value.
const SPAN_TOKEN: &str = "$siml::private::span$";
/// Name of magical field that can be used during deserialization to get the
/// source span of a value.
const SPANNED_TOKEN: &str = "$siml::private::span$";
/// Name of magical field that can be used during deserialization to get the
/// source location of a value.
const LOCATION_TOKEN: &str = "$siml::private::location$";

impl serde::ser::Error for Error {
    fn custom<T: std::fmt::Display>(msg: T) -> Self {
        Self::Custom(msg.to_string())
    }
}

pub fn from_spanned_value<'de, T: Deserialize<'de> + 'de>(
    value: &SpannedValue<'de>,
) -> Result<T, DeserializationError> {
    T::deserialize(value)
}

pub fn from_value<'de, T: Deserialize<'de> + 'de>(
    value: Value<'de>,
) -> Result<T, DeserializationError> {
    from_spanned_value(&value.to_spanned())
}

pub fn from_document<'de, T: Deserialize<'de> + 'de>(
    doc: &'de Document,
) -> Result<T, DeserializationError> {
    from_spanned_value(&doc.to_spanned_value())
}

pub fn from_bytes<T: for<'de> Deserialize<'de> + 'static>(
    bytes: &[u8],
) -> Result<T, DeserializationError> {
    let mut parser = ParseStream::new(bytes);
    let doc = Document::from_parser(&mut parser)?;
    from_document(&doc)
}

pub fn from_string<T: for<'de> Deserialize<'de> + 'static>(
    string: &str,
) -> Result<T, DeserializationError> {
    from_bytes(string.as_bytes())
}

pub fn to_document<T: Serialize + ?Sized>(value: &T) -> Result<Document, SerializationError> {
    value.serialize(RootSerializer::default())?.finish()
}

/// Serialize into an [`std::fmt::Write`] writer.
pub fn to_fmt_writer<T: Serialize + ?Sized, W: Write>(
    writer: W,
    value: &T,
) -> Result<(), SerializationError> {
    let document = to_document(value)?;
    let emitter = Emitter::new(writer);
    emitter.emit(&document)?;
    Ok(())
}

/// Serialize into an [`std::io::Write`] writer.
///
/// This function is guaranteed to only write valid UTF-8.
pub fn to_writer<T: Serialize + ?Sized, W: std::io::Write>(
    writer: W,
    value: &T,
) -> Result<(), SerializationError> {
    struct FmtWriter<W>(W);
    impl<W: std::io::Write> std::fmt::Write for FmtWriter<W> {
        fn write_str(&mut self, s: &str) -> std::fmt::Result {
            self.0.write_all(s.as_bytes()).map_err(|_| std::fmt::Error)
        }
    }

    to_fmt_writer(FmtWriter(writer), value)
}

/// Serialize into a string.
pub fn to_string<T: Serialize + ?Sized>(value: &T) -> Result<String, SerializationError> {
    let mut result = String::new();
    to_fmt_writer(&mut result, value)?;
    Ok(result)
}

/// Serialize into a byte vector.
///
/// This function is guaranteed to only produce valid UTF-8.
pub fn to_bytes<T: Serialize + ?Sized>(value: &T) -> Result<Vec<u8>, SerializationError> {
    to_string(value).map(|s| s.into_bytes())
}
