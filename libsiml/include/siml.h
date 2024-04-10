/**
 * SIML: Simon's Markup Language
 *
 * This is the header file for the C API. The implementation of the API is in
 * Rust.
 *
 * See README.md for more information.
 *
 */

#pragma once
#ifndef SIML_H_INCLUDED
#define SIML_H_INCLUDED

#include <stdint.h>

#ifdef __cplusplus
#define SIML_API extern "C"
#else
#define SIML_API
#endif

typedef struct _siml_document_s siml_document_t;
typedef struct _siml_parser_s siml_parser_t;
typedef struct _siml_scanner_s siml_scanner_t;
typedef struct _siml_emitter_s siml_emitter_t;
typedef struct _siml_event_s siml_event_t;
typedef struct _siml_token_s siml_token_t;
typedef struct _siml_error_s siml_error_t;
typedef uint64_t siml_node_id_t;

typedef enum siml_error_type_e {
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
} siml_error_type_t;

typedef enum siml_node_type_e {
  SIML_NODE_TYPE_SCALAR = 0,
  SIML_NODE_TYPE_LIST,
  SIML_NODE_TYPE_TUPLE,
  SIML_NODE_TYPE_MAPPING,
} siml_node_type_t;

typedef enum siml_scalar_style_e {
  SIML_SCALAR_STYLE_PLAIN = 0,
  SIML_SCALAR_STYLE_QUOTED,
  SIML_SCALAR_STYLE_SINGLE_QUOTED,
  SIML_SCALAR_STYLE_TRIMMED,
  SIML_SCALAR_STYLE_INDENT_TRIMMED,
} siml_scalar_style_t;

typedef enum siml_sequence_style_e {
  SIML_SEQUENCE_STYLE_MAPPING = 0,
  SIML_SEQUENCE_STYLE_LIST,
  SIML_SEQUENCE_STYLE_TUPLE,
} siml_sequence_style_t;

typedef enum siml_event_type_e {
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
} siml_event_type_t;

typedef enum siml_token_type_e {
  SIML_NO_TOKEN = 0,
  SIML_TOKEN_SCALAR,
  SIML_TOKEN_SEQUENCE_START_CURLY,
  SIML_TOKEN_SEQUENCE_END_CURLY,
  SIML_TOKEN_SEQUENCE_START_SQUARE,
  SIML_TOKEN_SEQUENCE_END_SQUARE,
  SIML_TOKEN_SEQUENCE_START_PARENS,
  SIML_TOKEN_SEQUENCE_END_PARENS,
  SIML_TOKEN_DELIMITER_COMMA,
  SIML_TOKEN_DELIMITER_NEWLINES,
  SIML_TOKEN_KEY_SIGIL,
  SIML_TOKEN_COLON,
  SIML_TOKEN_ANCHOR,
  SIML_TOKEN_REF,
  SIML_TOKEN_MERGE,
  SIML_TOKEN_COMMENT,
} siml_token_type_t;

typedef struct siml_location_s {
  size_t offset;
  size_t line;
  size_t column;
} siml_location_t;

typedef struct siml_span_s {
  siml_location_t start;
  siml_location_t end;
} siml_span_t;

typedef size_t (*siml_read_fn_t)(void *user_data, char *buffer,
                                 size_t buffer_len, int *out_raw_os_errno);
typedef size_t (*siml_write_fn_t)(void *user_data, const char *buffer,
                                  size_t buffer_len, int *out_raw_os_errno);

SIML_API siml_parser_t *siml_parser_new(void *user_data,
                                        siml_read_fn_t read_fn);
SIML_API siml_parser_t *siml_parser_from_utf8(const char *input, size_t len);
SIML_API siml_parser_t *siml_parser_from_c_string(const char *input);
SIML_API void siml_parser_delete(siml_parser_t *parser);
SIML_API int siml_parser_next_event(siml_parser_t *parser,
                                    siml_event_t *out_event,
                                    siml_error_t *out_err);

SIML_API siml_scanner_t *siml_scanner_new(void *user_data,
                                          siml_read_fn_t read_fn);
SIML_API void siml_scanner_delete(siml_scanner_t *scanner);
SIML_API int siml_scanner_next_token(siml_scanner_t *scanner,
                                     siml_token_t *out_token,
                                     siml_error_t *out_err);

SIML_API siml_emitter_t *siml_emitter_new(void *user_data,
                                          siml_write_fn_t write_fn);
SIML_API void siml_emitter_delete(siml_emitter_t *emitter);
SIML_API void siml_emitter_emit(siml_emitter_t *emitter,
                                const siml_document_t *doc, siml_error_t *err);

SIML_API siml_event_t *siml_event_new();
SIML_API void siml_event_delete(siml_event_t *event);
SIML_API int siml_event_is_some(const siml_event_t *event);
SIML_API siml_event_type_t siml_event_get_type(const siml_event_t *event);
SIML_API const char *siml_event_get_value(const siml_event_t *event,
                                          size_t *out_len);
SIML_API const char *siml_event_get_anchor(const siml_event_t *event,
                                           size_t *out_len);
SIML_API const char *siml_event_get_type_tag(const siml_event_t *event,
                                             size_t *out_len);
SIML_API void siml_event_get_span(const siml_event_t *event,
                                  siml_span_t *out_span);
SIML_API int siml_event_get_type_tag_span(const siml_event_t *event,
                                          siml_span_t *out_span);

SIML_API siml_token_t *siml_token_new();
SIML_API void siml_token_delete(siml_token_t *token);
SIML_API int siml_token_is_some(const siml_token_t *token);
SIML_API siml_token_type_t siml_token_get_type(const siml_token_t *token);
SIML_API const char *siml_token_get_value(const siml_token_t *token,
                                          size_t *out_len);
SIML_API void siml_token_get_span(const siml_token_t *token,
                                  siml_span_t *out_span);

SIML_API siml_document_t *siml_document_new();
SIML_API siml_document_t *siml_document_from_parser(siml_parser_t *parser,
                                                    siml_error_t *err);
SIML_API siml_document_t *siml_document_from_utf8(const char *input, size_t len,
                                                  siml_error_t *err);
SIML_API siml_document_t *siml_document_from_c_string(const char *input,
                                                      siml_error_t *err);
SIML_API void siml_document_delete(siml_document_t *doc);
SIML_API void siml_document_get_sequence(const siml_document_t *doc,
                                         siml_node_id_t id, size_t *out_len,
                                         siml_sequence_style_t *out_style,
                                         siml_error_t *err);
SIML_API void siml_document_get_sequence_item(const siml_document_t *doc,
                                              siml_node_id_t id, size_t index,
                                              siml_node_id_t *out_item_key,
                                              siml_node_id_t *out_item_value,
                                              siml_error_t *err);
SIML_API void siml_document_get_scalar(
    const siml_document_t *doc, siml_node_id_t id, const char **out_value,
    size_t *out_len, siml_scalar_style_t *out_style, const char **out_anchor,
    size_t *out_anchor_len, siml_error_t *err);
SIML_API siml_node_type_t siml_document_get_node_type(
    const siml_document_t *doc, siml_node_id_t id, siml_error_t *err);
SIML_API siml_node_id_t siml_document_add_scalar(
    siml_document_t *doc, const char *value, size_t value_len,
    const char *anchor, size_t anchor_len, siml_scalar_style_t style,
    siml_error_t *err);
SIML_API siml_node_id_t siml_document_add_sequence(
    siml_document_t *doc, siml_sequence_style_t style, const char *type_tag,
    size_t type_tag_len, const char *anchor, size_t anchor_len,
    siml_error_t *err);
SIML_API void siml_document_complete_sequence(siml_document_t *doc,
                                              siml_node_id_t id,
                                              siml_error_t *err);
SIML_API void siml_document_add_sequence_item(siml_document_t *doc,
                                              siml_node_id_t id,
                                              siml_node_id_t item_id,
                                              siml_error_t *err);

/**
 * @brief Create a new error object.
 *
 * Error objects may be created once and reused for any number of API calls.
 *
 * When an error object is no longer needed, it should be deleted with
 * `siml_error_delete()`.
 *
 * @return siml_error_t*
 */
SIML_API siml_error_t *siml_error_new();
/**
 * @brief Delete an error object.
 */
SIML_API void siml_error_delete(siml_error_t *err);
/**
 * @brief Check if an error object contains an error.
 *
 * This is equivalent to calling `siml_error_get_type()` and checking if the
 * return value is `SIML_NO_ERROR`.
 *
 * @return int 1 if the error object contains an error, 0 otherwise.
 */
SIML_API int siml_error_is_some(const siml_error_t *err);
/**
 * @brief If the error object contains an error, return the type of the error.
 *
 * Otherwise, returns `SIML_NO_ERROR`.
 *
 * @return siml_error_type_t
 */
SIML_API siml_error_type_t siml_error_get_type(const siml_error_t *err);
/**
 * @brief Get the error message, if any.
 *
 * This returns NULL if the error object does not contain an error.
 */
SIML_API const char *siml_error_get_message(const siml_error_t *err,
                                            size_t *out_len);
/**
 * @brief Get the location span of the error.
 *
 * This is the logical span of source code that caused the error.
 *
 * If the error object does not contain an error, the span will be empty, and
 * the function returns 0.
 */
SIML_API int siml_error_get_span(const siml_error_t *err,
                                 siml_span_t *out_span);

#endif
