use std::{ffi::c_char, num::NonZeroUsize, os::raw::c_void};

use libc::{c_int, size_t};
use siml::{
    private::*, BuilderError, CachedEvent, Document, Error, Event, EventType, ParseStream,
    ParserError, Scalar, ScalarStyle, ScannerError, SequenceStyle,
};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
#[allow(non_camel_case_types)]
pub enum ApiErrorType {
    #[default]
    SIML_NO_ERROR = 0,
    SIML_ERROR_IO,
    SIML_ERROR_UNEXPECTED_EOF,
    SIML_ERROR_UNEXPECTED_CHAR,
    SIML_ERROR_INVALID_ESCAPE_SEQUENCE,
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

    SIML_ERROR_PARSER_MISSING_VALUE = 100,
    SIML_ERROR_PARSER_UNCLOSED_DELIMITER,
    SIML_ERROR_PARSER_UNEXPECTED_TOKEN,
    SIML_ERROR_PARSER_MISSING_COMMA_IN_LIST,
    SIML_ERROR_PARSER_TRAILING_ANCHOR,
    SIML_ERROR_PARSER_MULTIPLE_ANCHORS,
    SIML_ERROR_PARSER_REFERENCE_WITH_ANCHOR,

    SIML_ERROR_OTHER = 999,
}
use ApiErrorType::*;

#[derive(Default)]
pub struct ApiError {
    ty: ApiErrorType,
    message: String,
}

impl ApiError {
    pub fn set_none(&mut self) {
        self.ty = SIML_NO_ERROR;
    }

    pub fn set<T: Into<Error>>(&mut self, err: T) {
        use std::fmt::Write;
        let err = err.into();
        self.ty = ApiErrorType::from(&err);
        self.message.clear();
        _ = write!(&mut self.message, "{}", err);
    }
}

impl<T: Into<Error>> From<T> for ApiError {
    fn from(value: T) -> Self {
        let mut err = ApiError::default();
        err.set(value);
        err
    }
}

impl From<&Error> for ApiErrorType {
    fn from(value: &Error) -> Self {
        match value {
            Error::Io(_) => SIML_ERROR_IO,
            Error::Utf8(_) => SIML_ERROR_INVALID_UTF8,
            Error::Scanner(ref err) => err.into(),
            Error::Parser(ref err) => err.into(),
            Error::Builder(ref err) => err.into(),
            Error::Custom(_) => SIML_ERROR_OTHER,
        }
    }
}

impl From<&BuilderError> for ApiErrorType {
    fn from(value: &BuilderError) -> Self {
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

impl From<&ScannerError> for ApiErrorType {
    fn from(value: &ScannerError) -> Self {
        match value {
            ScannerError::UnexpectedChar(_) => SIML_ERROR_UNEXPECTED_CHAR,
            ScannerError::InvalidEscapeSequence(_) => SIML_ERROR_INVALID_ESCAPE_SEQUENCE,
            ScannerError::UnexpectedEof => SIML_ERROR_UNEXPECTED_EOF,
        }
    }
}

impl From<&ParserError> for ApiErrorType {
    fn from(value: &ParserError) -> Self {
        match value {
            ParserError::MissingValue => SIML_ERROR_PARSER_MISSING_VALUE,
            ParserError::UnclosedDelimiter(_, _) => SIML_ERROR_PARSER_UNCLOSED_DELIMITER,
            ParserError::UnexpectedToken(_, _) => SIML_ERROR_PARSER_UNEXPECTED_TOKEN,
            ParserError::MissingCommaInList(_) => SIML_ERROR_PARSER_MISSING_COMMA_IN_LIST,
            ParserError::TrailingAnchor(_) => SIML_ERROR_PARSER_TRAILING_ANCHOR,
            ParserError::MultipleAnchors(_) => SIML_ERROR_PARSER_MULTIPLE_ANCHORS,
            ParserError::ReferenceWithAnchor(_) => SIML_ERROR_PARSER_REFERENCE_WITH_ANCHOR,
        }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
#[allow(non_camel_case_types)]
pub enum ApiEventType {
    #[default]
    SIML_NO_EVENT = 0,
    SIML_EVENT_SCALAR,
    SIML_EVENT_REF,
    SIML_EVENT_MERGE,
    SIML_EVENT_SEQUENCE_START,
    SIML_EVENT_SEQUENCE_END,
    SIML_EVENT_MAPPING_START,
    SIML_EVENT_MAPPING_END,
    SIML_EVENT_EMPTY_KEY_OR_VALUE,
    SIML_EVENT_COMMENT,
}
use ApiEventType::*;

impl From<EventType> for ApiEventType {
    fn from(value: EventType) -> Self {
        match value {
            EventType::Scalar(_) => SIML_EVENT_SCALAR,
            EventType::Ref => SIML_EVENT_REF,
            EventType::Merge => SIML_EVENT_MERGE,
            EventType::BeginSequence(_) => SIML_EVENT_SEQUENCE_START,
            EventType::EndSequence => SIML_EVENT_SEQUENCE_END,
            EventType::BeginMapping => SIML_EVENT_MAPPING_START,
            EventType::EndMapping => SIML_EVENT_MAPPING_END,
            EventType::Empty => SIML_EVENT_EMPTY_KEY_OR_VALUE,
            EventType::Comment => SIML_EVENT_COMMENT,
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

impl From<std::str::Utf8Error> for ApiErrorType {
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

unsafe fn api_try<F: FnOnce() -> Result<R, Error>, R: Default>(err: *mut ApiError, f: F) -> R {
    api_try_or(err, f, R::default)
}

unsafe fn api_try_or<F: FnOnce() -> Result<R, Error>, D: FnOnce() -> R, R>(
    err: *mut ApiError,
    f: F,
    default: D,
) -> R {
    if !err.is_null() {
        (*err).set_none();
    }
    let unwind = std::panic::catch_unwind(std::panic::AssertUnwindSafe(move || match f() {
        Ok(r) => r,
        Err(e) => {
            if !err.is_null() {
                (*err).set(e);
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

unsafe fn not_null<'a, T>(ptr: *const T) -> &'a T {
    if ptr.is_null() {
        panic!("null pointer")
    } else {
        &*ptr
    }
}
unsafe fn not_null_mut<'a, T>(ptr: *mut T) -> &'a mut T {
    if ptr.is_null() {
        panic!("null pointer")
    } else {
        &mut *ptr
    }
}

fn get_node(doc: &Document, node_id: ApiNodeId) -> Option<&Node> {
    node_id.to_node_id().map(|node_id| doc.get_node(node_id))
}

fn get_seq(doc: &Document, node_id: ApiNodeId) -> &SequenceNode {
    match get_node(doc, node_id) {
        Some(Node {
            data: NodeData::Sequence(seq),
            ..
        }) => seq,
        Some(_) => panic!("provided node ID does not refer to a sequence"),
        None => doc.get_root(),
    }
}

fn get_scalar(doc: &Document, node_id: ApiNodeId) -> Scalar {
    match get_node(doc, node_id) {
        Some(Node {
            data: NodeData::Scalar(ScalarNode { value, style }),
            anchor,
            span: _,
        }) => Scalar {
            value: value.as_str(),
            style: *style,
            anchor: anchor.as_ref().map(|anchor| anchor.as_str()),
        },
        None | Some(_) => panic!("provided node ID does not refer to a scalar"),
    }
}

#[allow(non_camel_case_types)]
type siml_parser_t = ParseStream<ApiBufRead>;
type ApiBufRead = std::io::BufReader<ApiRead>;

pub enum ApiRead {
    Custom {
        user_data: *mut c_void,
        read_fn: unsafe extern "C" fn(*mut c_void, *mut u8, size_t, *mut c_int) -> size_t,
    },
    Buffer(std::io::Cursor<Vec<u8>>),
}

impl std::io::Read for ApiRead {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        match self {
            ApiRead::Custom { user_data, read_fn } => unsafe {
                // SAFETY: It is up to the user of the API to ensure that
                // read_fn is safe to call.
                let mut err = 0;
                let read = read_fn(*user_data, buf.as_mut_ptr(), buf.len(), &mut err);
                if err != 0 {
                    Err(std::io::Error::from_raw_os_error(err))
                } else {
                    Ok(read)
                }
            },
            ApiRead::Buffer(in_buf) => std::io::Read::read(in_buf, buf),
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn siml_parser_new(
    user_data: *mut c_void,
    read_fn: Option<unsafe extern "C" fn(*mut c_void, *mut u8, size_t, *mut c_int) -> size_t>,
) -> *mut siml_parser_t {
    let read_fn = read_fn.expect("read_fn must not be NULL");
    Box::into_raw(Box::new(ParseStream::new(std::io::BufReader::new(
        ApiRead::Custom { user_data, read_fn },
    ))))
}

#[no_mangle]
pub unsafe extern "C" fn siml_parser_from_utf8(
    input: *const c_char,
    len: size_t,
) -> *mut siml_parser_t {
    let input = unsafe { std::slice::from_raw_parts(input as *const u8, len) };
    Box::into_raw(Box::new(ParseStream::new(std::io::BufReader::new(
        ApiRead::Buffer(std::io::Cursor::new(input.to_vec())),
    ))))
}

#[no_mangle]
pub unsafe extern "C" fn siml_parser_from_c_string(input: *const c_char) -> *mut siml_parser_t {
    let input = unsafe { std::ffi::CStr::from_ptr(input) };
    let input = input.to_bytes();
    siml_parser_from_utf8(input.as_ptr() as _, input.len() as _)
}

#[no_mangle]
pub unsafe extern "C" fn siml_parser_next_event(
    parser: *mut siml_parser_t,
    event: *mut CachedEvent,
    err: *mut ApiError,
) -> c_int {
    api_try(err, || unsafe {
        let parser = not_null_mut(parser);
        let event = not_null_mut(event);
        parser.next_event(event)?;
        Ok(1)
    })
}

#[no_mangle]
pub unsafe extern "C" fn siml_parser_delete(parser: *mut siml_parser_t) {
    if parser.is_null() {
        return;
    }
    unsafe {
        _ = Box::from_raw(parser);
    }
}

#[no_mangle]
pub extern "C" fn siml_event_new() -> *mut CachedEvent {
    Box::into_raw(Box::new(CachedEvent::default()))
}

#[no_mangle]
pub unsafe extern "C" fn siml_event_delete(event: *mut CachedEvent) {
    if event.is_null() {
        return;
    }
    unsafe {
        _ = Box::from_raw(event);
    }
}

#[no_mangle]
pub unsafe extern "C" fn siml_event_is_some(event: *const CachedEvent) -> c_int {
    if event.is_null() {
        0
    } else {
        (*event).is_some() as _
    }
}

#[no_mangle]
pub unsafe extern "C" fn siml_event_get_type(event: *const CachedEvent) -> ApiEventType {
    let event = if event.is_null() {
        return SIML_NO_EVENT;
    } else {
        &*event
    };
    let Some(ty) = event.ty() else {
        return SIML_NO_EVENT;
    };
    ty.into()
}

#[no_mangle]
pub unsafe extern "C" fn siml_event_get_value(
    event: *const CachedEvent,
    len: *mut size_t,
) -> *const u8 {
    if !len.is_null() {
        *len = 0;
    }

    let event = if event.is_null() {
        return std::ptr::null();
    } else {
        &*event
    };
    let Some(event) = event.get() else {
        return std::ptr::null();
    };
    let value: &str = match event {
        Event::Scalar(scalar) => scalar.value.value,
        Event::Ref(v) | Event::Merge(v) => v.value,
        Event::BeginSequence { .. }
        | Event::EndSequence(_)
        | Event::BeginMapping { .. }
        | Event::EndMapping(_)
        | Event::Empty(_) => "",
        Event::Comment(comment) => comment.value,
    };
    if !len.is_null() {
        *len = value.len();
    }
    value.as_ptr()
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
        let doc = not_null(doc);
        Ok(get_seq(doc, node).items.len())
    })
}

#[no_mangle]
pub unsafe extern "C" fn siml_document_get_node_type(
    doc: *const Document,
    node: ApiNodeId,
    err: *mut ApiError,
) -> ApiNodeType {
    api_try(err, || unsafe {
        let doc = not_null(doc);
        Ok(
            get_node(doc, node).map_or(SIML_NODE_TYPE_MAPPING, |node| match &node.data {
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
        let doc = not_null_mut(doc);
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
        let doc = not_null(doc);
        let scalar = get_scalar(doc, node);
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
        let doc = not_null(doc);
        Ok(get_scalar(doc, node).style.into())
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
            let doc = not_null(doc);
            let scalar = get_scalar(doc, node);
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
        let doc = not_null_mut(doc);
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
        let doc = not_null_mut(doc);
        if let Some(seq) = seq.to_node_id() {
            doc.complete_sequence(seq);
        }
        Ok(())
    })
}

#[no_mangle]
pub extern "C" fn siml_error_new() -> *mut ApiError {
    Box::into_raw(Box::new(ApiError::default()))
}

#[no_mangle]
pub unsafe extern "C" fn siml_error_delete(err: *mut ApiError) {
    if err.is_null() {
        return;
    }
    unsafe {
        _ = Box::from_raw(err);
    }
}

#[no_mangle]
pub unsafe extern "C" fn siml_error_get_type(err: *const ApiError) -> ApiErrorType {
    if err.is_null() {
        SIML_NO_ERROR
    } else {
        (*err).ty
    }
}

#[no_mangle]
pub unsafe extern "C" fn siml_error_get_message(
    err: *const ApiError,
    len: *mut size_t,
) -> *const c_char {
    let err = if err.is_null() {
        return std::ptr::null();
    } else {
        &*err
    };
    if !len.is_null() {
        *len = err.message.as_bytes().len();
    }
    err.message.as_bytes().as_ptr() as _
}

#[no_mangle]
pub unsafe extern "C" fn siml_error_is_some(err: *const ApiError) -> c_int {
    if err.is_null() {
        0
    } else {
        ((*err).ty != SIML_NO_ERROR) as _
    }
}
