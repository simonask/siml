#![cfg(feature = "c_api")]

use std::num::NonZeroUsize;

use crate::{
    BuilderError, Document, Node, NodeData, NodeId, Scalar, ScalarStyle, SequenceNode,
    SequenceStyle,
};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
#[allow(non_camel_case_types)]
pub enum ApiError {
    #[default]
    SIML_NO_ERROR = 0,
    SIML_ERROR_NULL_POINTER,
    SIML_ERROR_INVALID_NODE_ID,
    SIML_ERROR_NOT_A_SEQUENCE,
    SIML_ERROR_NOT_A_SCALAR,
    SIML_ERROR_INVALID_UTF8,
    SIML_ERROR_UNDEFINED_ANCHOR,
    SIML_ERROR_INCOMPLETE_ANCHOR,
    SIML_ERROR_DUPLICATE_ANCHOR,
    SIML_ERROR_MULTIPLE_ANCHORS,
    SIML_ERROR_UNEXPECTED_EVENT,
    SIML_ERROR_MERGE_INTO_NON_SEQUENCE,
}
use ApiError::*;

impl From<BuilderError> for ApiError {
    fn from(value: BuilderError) -> Self {
        match value {
            BuilderError::UndefinedAnchor(_) => SIML_ERROR_UNDEFINED_ANCHOR,
            BuilderError::IncompleteAnchor(_) => SIML_ERROR_INCOMPLETE_ANCHOR,
            BuilderError::DuplicateAnchor(_) => SIML_ERROR_DUPLICATE_ANCHOR,
            BuilderError::MultipleAnchors(..) => SIML_ERROR_MULTIPLE_ANCHORS,
            BuilderError::UnexpectedEvent => SIML_ERROR_UNEXPECTED_EVENT,
            BuilderError::MergeIntoNonSequence => SIML_ERROR_MERGE_INTO_NON_SEQUENCE,
        }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
#[allow(non_camel_case_types)]
pub enum ApiNodeType {
    #[default]
    SIML_NODE_TYPE_SCALAR = 0,
    SIML_NODE_TYPE_LIST,
    SIML_NODE_TYPE_TUPLE,
    SIML_NODE_TYPE_MAPPING,
}
use ApiNodeType::*;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
#[allow(non_camel_case_types)]
pub enum ApiScalarStyle {
    #[default]
    SIML_SCALAR_STYLE_PLAIN = 0,
    SIML_SCALAR_STYLE_QUOTED,
    SIML_SCALAR_STYLE_SINGLE_QUOTED,
    SIML_SCALAR_STYLE_TRIMMED,
    SIML_SCALAR_STYLE_INDENT_TRIMMED,
}
use ApiScalarStyle::*;

impl From<ScalarStyle> for ApiScalarStyle {
    fn from(style: ScalarStyle) -> Self {
        match style {
            ScalarStyle::Plain => SIML_SCALAR_STYLE_PLAIN,
            ScalarStyle::Quoted => SIML_SCALAR_STYLE_QUOTED,
            ScalarStyle::SingleQuoted => SIML_SCALAR_STYLE_SINGLE_QUOTED,
            ScalarStyle::QuotedTrimWhitespace => SIML_SCALAR_STYLE_TRIMMED,
            ScalarStyle::QuotedTrimIndent => SIML_SCALAR_STYLE_INDENT_TRIMMED,
        }
    }
}

impl From<ApiScalarStyle> for ScalarStyle {
    fn from(style: ApiScalarStyle) -> Self {
        match style {
            SIML_SCALAR_STYLE_PLAIN => ScalarStyle::Plain,
            SIML_SCALAR_STYLE_QUOTED => ScalarStyle::Quoted,
            SIML_SCALAR_STYLE_SINGLE_QUOTED => ScalarStyle::SingleQuoted,
            SIML_SCALAR_STYLE_TRIMMED => ScalarStyle::QuotedTrimWhitespace,
            SIML_SCALAR_STYLE_INDENT_TRIMMED => ScalarStyle::QuotedTrimIndent,
        }
    }
}

impl From<std::str::Utf8Error> for ApiError {
    fn from(_: std::str::Utf8Error) -> Self {
        SIML_ERROR_INVALID_UTF8
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct ApiNodeId(u64);

impl From<NodeId> for ApiNodeId {
    fn from(value: NodeId) -> Self {
        ApiNodeId(value.0.get() as _)
    }
}

impl From<ApiNodeId> for Option<NodeId> {
    fn from(value: ApiNodeId) -> Self {
        value.to_node_id()
    }
}

impl ApiNodeId {
    fn to_node_id(self) -> Option<NodeId> {
        NonZeroUsize::new(self.0 as _).map(NodeId)
    }
}

unsafe fn api_try<F: FnOnce() -> Result<R, ApiError>, R: Default>(err: *mut ApiError, f: F) -> R {
    api_try_or(err, f, R::default)
}

unsafe fn api_try_or<F: FnOnce() -> Result<R, ApiError>, D: FnOnce() -> R, R>(
    err: *mut ApiError,
    f: F,
    default: D,
) -> R {
    if !err.is_null() {
        *err = SIML_NO_ERROR;
    }
    let unwind = std::panic::catch_unwind(std::panic::AssertUnwindSafe(move || match f() {
        Ok(r) => r,
        Err(e) => {
            if !err.is_null() {
                *err = e;
            }
            default()
        }
    }));
    match unwind {
        Ok(value) => value,
        Err(err) => unsafe {
            eprintln!("Assertion failed: {err:?}");
            libc::abort()
        },
    }
}

unsafe fn not_null<'a, T>(ptr: *const T) -> Result<&'a T, ApiError> {
    if ptr.is_null() {
        Err(SIML_ERROR_NULL_POINTER)
    } else {
        Ok(&*ptr)
    }
}
unsafe fn not_null_mut<'a, T>(ptr: *mut T) -> Result<&'a mut T, ApiError> {
    if ptr.is_null() {
        Err(SIML_ERROR_NULL_POINTER)
    } else {
        Ok(&mut *ptr)
    }
}

fn get_node(doc: &Document, node_id: ApiNodeId) -> Result<Option<&Node>, ApiError> {
    match node_id.to_node_id() {
        None => Ok(None),
        Some(node_id) => Ok(Some(
            doc.nodes
                .get(node_id.0.get() - 1)
                .ok_or(SIML_ERROR_INVALID_NODE_ID)?,
        )),
    }
}

fn get_seq(doc: &Document, node_id: ApiNodeId) -> Result<&SequenceNode, ApiError> {
    match get_node(doc, node_id)? {
        Some(Node {
            data: NodeData::Sequence(seq),
            ..
        }) => Ok(seq),
        Some(_) => Err(SIML_ERROR_NOT_A_SEQUENCE),
        None => Ok(&doc.sequence),
    }
}

fn get_scalar(doc: &Document, node_id: ApiNodeId) -> Result<Scalar, ApiError> {
    match get_node(doc, node_id)? {
        Some(Node {
            data: NodeData::Scalar(scalar),
            ..
        }) => Ok(scalar.borrow()),
        None | Some(_) => Err(SIML_ERROR_NOT_A_SCALAR),
    }
}

#[no_mangle]
pub extern "C" fn siml_document_new() -> *mut Document {
    Box::into_raw(Box::new(Document::default()))
}

#[no_mangle]
pub unsafe extern "C" fn siml_document_delete(doc: *mut Document) {
    if doc.is_null() {
        return;
    }
    _ = Box::from_raw(doc);
}

#[no_mangle]
pub unsafe extern "C" fn siml_document_len(
    doc: *const Document,
    node: ApiNodeId,
    err: *mut ApiError,
) -> usize {
    api_try(err, || unsafe {
        let doc = not_null(doc)?;
        Ok(get_seq(doc, node)?.items.len())
    })
}

#[no_mangle]
pub unsafe extern "C" fn siml_document_get_node_type(
    doc: *const Document,
    node: ApiNodeId,
    err: *mut ApiError,
) -> ApiNodeType {
    api_try(err, || unsafe {
        let doc = not_null(doc)?;
        Ok(
            get_node(doc, node)?.map_or(SIML_NODE_TYPE_MAPPING, |node| match &node.data {
                NodeData::Scalar(_) => SIML_NODE_TYPE_SCALAR,
                NodeData::Sequence(seq) => match seq.style {
                    SequenceStyle::List => SIML_NODE_TYPE_LIST,
                    SequenceStyle::Tuple => SIML_NODE_TYPE_TUPLE,
                    SequenceStyle::Mapping => SIML_NODE_TYPE_MAPPING,
                },
            }),
        )
    })
}

#[no_mangle]
pub unsafe extern "C" fn siml_document_add_scalar(
    doc: *mut Document,
    value: *const u8,
    value_len: usize,
    anchor: *const u8,
    anchor_len: usize,
    style: ApiScalarStyle,
    err: *mut ApiError,
) -> ApiNodeId {
    api_try(err, || unsafe {
        let doc = not_null_mut(doc)?;
        let value = std::str::from_utf8(std::slice::from_raw_parts(value, value_len))?;
        let anchor = if anchor.is_null() {
            None
        } else {
            Some(std::str::from_utf8(std::slice::from_raw_parts(
                anchor, anchor_len,
            ))?)
        };
        let scalar = Scalar {
            value,
            anchor,
            style: style.into(),
        };
        doc.add_scalar(scalar).map(Into::into).map_err(Into::into)
    })
}

#[no_mangle]
pub unsafe extern "C" fn siml_document_get_scalar(
    doc: *const Document,
    node: ApiNodeId,
    value: *mut *const u8,
    value_len: *mut usize,
    anchor: *mut *const u8,
    anchor_len: *mut usize,
    style: *mut ApiScalarStyle,
    err: *mut ApiError,
) -> libc::c_int {
    api_try(err, || unsafe {
        let doc = not_null(doc)?;
        let scalar = get_scalar(doc, node)?;
        if !value.is_null() {
            *value = scalar.value.as_bytes().as_ptr();
        }
        if !value_len.is_null() {
            *value_len = scalar.value.as_bytes().len();
        }
        if !anchor.is_null() {
            *anchor = scalar
                .anchor
                .as_deref()
                .map_or(std::ptr::null(), |a| a.as_bytes().as_ptr());
        }
        if !anchor_len.is_null() {
            *anchor_len = scalar.anchor.as_ref().map_or(0, |a| a.as_bytes().len());
        }
        if !style.is_null() {
            *style = scalar.style.into();
        }

        Ok(1)
    })
}

#[no_mangle]
pub unsafe extern "C" fn siml_document_get_scalar_style(
    doc: *const Document,
    node: ApiNodeId,
    err: *mut ApiError,
) -> ApiScalarStyle {
    api_try(err, || unsafe {
        let doc = not_null(doc)?;
        Ok(get_scalar(doc, node)?.style.into())
    })
}

#[no_mangle]
pub unsafe extern "C" fn siml_document_get_scalar_value(
    doc: *const Document,
    node: ApiNodeId,
    len: *mut usize,
    err: *mut ApiError,
) -> *const u8 {
    api_try_or(
        err,
        || unsafe {
            let doc = not_null(doc)?;
            let scalar = get_scalar(doc, node)?;
            *len = scalar.value.as_bytes().len();
            Ok(scalar.value.as_bytes().as_ptr())
        },
        std::ptr::null,
    )
}

#[no_mangle]
pub unsafe extern "C" fn siml_document_add_sequence(
    doc: *mut Document,
    style: ApiNodeType,
    anchor: *const u8,
    anchor_len: usize,
    type_tag: *const u8,
    type_tag_len: usize,
    err: *mut ApiError,
) -> ApiNodeId {
    let style = match style {
        SIML_NODE_TYPE_LIST => SequenceStyle::List,
        SIML_NODE_TYPE_TUPLE => SequenceStyle::Tuple,
        SIML_NODE_TYPE_MAPPING => SequenceStyle::Mapping,
        _ => {
            return siml_document_add_scalar(
                doc,
                b"" as _,
                0,
                anchor,
                anchor_len,
                SIML_SCALAR_STYLE_PLAIN,
                err,
            )
        }
    };

    api_try(err, || unsafe {
        let doc = not_null_mut(doc)?;
        let anchor = if anchor.is_null() {
            None
        } else {
            Some(std::str::from_utf8(std::slice::from_raw_parts(
                anchor, anchor_len,
            ))?)
        };
        let type_tag = if type_tag.is_null() {
            None
        } else {
            Some(std::str::from_utf8(std::slice::from_raw_parts(
                type_tag,
                type_tag_len,
            ))?)
        };
        let type_tag = type_tag
            .map(|type_tag| doc.add_scalar(Scalar::plain(type_tag)))
            .transpose()?;
        doc.add_sequence(style, anchor, type_tag)
            .map(Into::into)
            .map_err(Into::into)
    })
}

#[no_mangle]
pub unsafe extern "C" fn siml_document_complete_sequence(
    doc: *mut Document,
    seq: ApiNodeId,
    err: *mut ApiError,
) {
    api_try(err, || unsafe {
        let doc = not_null_mut(doc).unwrap();
        if let Some(seq) = seq.to_node_id() {
            doc.complete_sequence(seq);
        }
        Ok(())
    })
}
