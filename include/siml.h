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
typedef uint64_t siml_node_id_t;

typedef enum siml_error_e {
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
} siml_error_t;

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

SIML_API siml_document_t *siml_document_new();
SIML_API void siml_document_delete(siml_document_t *doc);
SIML_API size_t siml_document_len(const siml_document_t *doc, siml_node_id_t id,
                                  siml_error_t *err);
SIML_API siml_node_type_t siml_document_get_node_type(
    const siml_document_t *doc, siml_node_id_t id, siml_error_t *err);
SIML_API siml_node_id_t siml_document_add_scalar(
    siml_document_t *doc, const char *value, size_t value_len,
    const char *anchor, size_t anchor_len, siml_scalar_style_t style,
    siml_error_t *err);

#endif
