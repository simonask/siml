#include "siml.h"
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

static void parse_basic() {
    siml_parser_t * parser = siml_parser_from_c_string("foo");
    siml_event_t *event = siml_event_new();
    siml_error_t *error = siml_error_new();

    siml_parser_next_event(parser, event, error);
    assert(!siml_error_is_some(error));
    assert(siml_event_get_type(event) == SIML_EVENT_EMPTY_KEY_OR_VALUE);

    siml_parser_next_event(parser, event, error);
    assert(!siml_error_is_some(error));
    assert(siml_event_get_type(event) == SIML_EVENT_SCALAR);
    size_t len;
    const char* value = siml_event_get_value(event, &len);
    assert(strncmp(value, "foo", len) == 0);

    siml_parser_next_event(parser, event, error);
    assert(!siml_event_is_some(event));

cleanup:
    siml_error_delete(error);
    siml_event_delete(event);
    siml_parser_delete(parser);
}

int main() {
#if defined(_WIN32) || defined(WIN32)
  _set_error_mode(1);
#endif

  parse_basic();
  return 0;
}
