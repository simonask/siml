//! Simon's Markup Language

mod char;
mod document;
mod error;
mod event;
mod input_buffer;
mod location;
mod parser;
mod scalar;
mod scanner;
mod token;
mod value;

mod builder;
mod emitter;
#[cfg(feature = "serde")]
mod serialization;

pub use builder::*;
pub use char::*;
pub use document::*;
pub use emitter::*;
pub use error::*;
pub use event::*;
use input_buffer::*;
pub use location::*;
pub use parser::*;
pub use scalar::*;
pub use scanner::*;
pub use token::*;
pub use value::*;

#[cfg(feature = "serde")]
pub use serialization::*;
