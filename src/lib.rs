//! Simon's Markup Language

pub mod builder;
pub mod char;
mod document;
pub mod emitter;
mod error;
mod event;
mod input_buffer;
pub mod location;
pub mod parser;
mod scalar;
mod scanner;
mod serialization;
pub mod string;
mod token;
mod value;

pub use builder::{
    BuildAsList, BuildAsMapping, BuildAsTuple, BuildSeq, BuildValue, Builder, BuilderError,
};
use char::*;
pub use document::{Document, NodeId};
pub use emitter::Emitter;
pub use error::*;
pub use event::*;
use input_buffer::*;
use location::*;
pub use parser::*;
pub use scalar::*;
pub use scanner::*;
use string::*;
pub use token::*;
pub use value::*;

#[cfg(feature = "serde")]
pub use serialization::*;

#[doc(hidden)]
pub mod private {
    pub use crate::document::{Node, NodeData, NodeId, ScalarNode, SequenceItem, SequenceNode};
}

pub mod prelude {
    #[doc(no_inline)]
    pub use crate::{
        char::CharExt as _, string::StringExt as _, BuildAsList, BuildAsMapping, BuildAsTuple,
        BuildSeq as _, BuildValue as _, Builder, BuilderError, Document, Emitter, Event,
        ParseStream, Scalar, ScalarStyle, Scanner, Token, Value,
    };
}
