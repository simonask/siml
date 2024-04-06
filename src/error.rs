use crate::{BuilderError, ParserError, ScannerError};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    Io(#[from] std::io::Error),
    #[error(transparent)]
    Utf8(#[from] std::str::Utf8Error),
    #[error(transparent)]
    Scanner(#[from] ScannerError),
    #[error(transparent)]
    Parser(#[from] ParserError),
    #[error(transparent)]
    Builder(#[from] BuilderError),
    #[error("{0}")]
    Custom(String),
}

impl PartialEq<ScannerError> for Error {
    fn eq(&self, other: &ScannerError) -> bool {
        if let Error::Scanner(err) = self {
            *err == *other
        } else {
            false
        }
    }
}

impl PartialEq<ParserError> for Error {
    fn eq(&self, other: &ParserError) -> bool {
        if let Error::Parser(err) = self {
            *err == *other
        } else {
            false
        }
    }
}
