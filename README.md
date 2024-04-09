# Simon's Markup Language

SIML is a spiritual successor to YAML, intended for the same niches, but with a
different syntax, and a slightly different feature set.

SIML's syntax is optimized for humans, and not for efficient machine-to-machine
communication. It is highly suitable for things like configuration files and
structured XML/HTML-like markup. That said, the grammar is very simple and very
easy for a computer to parse, meaning that the code size of the parser is quite
small.

SIML's syntax is a "superset" of JSON, similar to YAML 1.2. All valid JSON
can also be parsed as valid SIML.

## Features

- Optimized for human readability.

- Struct-like syntax with built-in type tags:

  - `MyTypeTag { field1: 123.0, field2: "string" }`

- Built-in support for internal references.

  - Prefix any value with `@my_name` to give it a name.

  - Refer to the value with `&my_name`, or `*my_name` to merge it into the current mapping/sequence.

- Simple grammar, very fast recursive descent parser.

- **Rust-specific:** Built-in support for `serde`.

## Planned features

- High-level C API for interoperability with other languages.

## What does it look like?

Here are some translated examples from https://json.org/example.html.

```siml
glossary: {
    title: "example glossary

    GlossEntry {
        ID: SGML
        SortAs: SGML
        Acronym: SGML
        Abbrev: "ISO 8879:1986"
        GlossDef: {
            para: "A meta-markup language, used to create markup languages such as DocBook"
        }
        GlossSee: markup
    }
}
```

```siml
menu: {
    header: "SVG Viewer"

    # Items don't need keys.

    { id: Open }
    { id: OpenNew, label: "Open New" }
    Separator                          # Identifiers can be items
    { id: ZoomIn, label: "Zoom In" }
    { id: ZoomOut, label: "Zoom Out" }
    { id: OriginalView, label: "Original View" }
    # ...
}
```

## Stringly typed

One of the major differences from JSON, and to some extent YAML, is that all
scalar values are strings from the perspective of the parser. Instead,
validation and conversion to/from other data types is delegated to whatever
serialization framework is used on top of SIML.

This gives significant freedom to design domain-specific schemas for SIML
documents, and avoids pitfalls where the serialization format imposes specific
restrictions on the data model.

For example, one famous shortcoming of YAML is that the character sequence `no`
is parsed as boolean `false`, but this might be unexpected in many contexts,
like a list of country codes, where it could also mean "Norway".

Another example is numeric types, where tying a data model to a particular
representation is often limiting. For example, you might want big numbers to
look like numbers, even if they can't be represented by an `i64` or an `f64`.

Given this, SIML is only *structurally* "self-describing" (sequences and
mappings), prioritizing human-friendliness over validation.

**Rust-specific:** The implementation of `serde::Deserializer` supports
`deserialize_any()`, but the deserializer makes an educated guess about the
format, where it tries to parse scalars as booleans and numeric types, and
deserializes a string if those fail. This may not be what you want, so it is
good practice to avoid `deserialize_any()` when possible.

## Comparison with YAML and JSON

- Identifier-like or number-like strings do not need to be in quotes (similar to
  "plain" scalars in YAML). However, they cannot contain whitespace.
- Scalars are strings. Parsing of numbers, booleans, etc. is handled by the
  higher-level deserializer. This means that there are no "special" strings in
  the syntax (`true` means the same as `"true"`), and all scalar values can
  always be deserialized as strings.
- Indentation is not significant. Sequences and mappings are always surrounded
  by delimiters such as `{...}`, `[...]`, or `(...)`, like JSON.
- Newlines _are_ significant within mappings, where it can be the delimiter
  between entries instead of comma.
- Duplicate keys are explicitly allowed in mappings, and no attempt is made to
  prevent that use case.
- Mappings are a hybrid between key-value pairs and sequences, meaning that the
  syntax explicitly supports entries in mappings without keys. This is to
  support the common (XML-like) pattern of having objects that have both
  attributes and "contents", which would otherwise only be possible in JSON and
  YAML by having a special `items` attribute.
  - To explicitly indicate an item without a key in a mapping, use the syntax
    `?: value`.
  - To explicitly indicate the presence of a key in a key-value pair, use the
    syntax `?key: value`.
  - Complex keys (mappings and sequences) must always be preceded by `?`, like
    `?[1,2,3]: value`[^1].
- The root of a document is always logically a mapping, but that mapping can
  have a single scalar child element. The input `foo` is parsed as `{ ?: foo }`.
- It is possible to distinguish between lists and tuples, by choosing either
  square brackets or parentheses when typing a sequence. Deserializers may
  choose to consider the style of the sequence significant or not.
- Sequences and mappings have explicit support in the syntax for a "type tag",
  which makes them look like Rust expressions. This is particularly handy for
  any heterogenously typed container.
  - The implementation of `serde::Serializer` and `serde::Deserializer` uses
    this feature to indicate enum variants.
- Complex keys (sequences and mappings) are supported in mappings, by prefixing
  the key with `?` (like YAML).
- Trimmed and folded string literals are supported.
    - `-"..."` trims all leading and trailing whitespace.
    - `|"..."` trims the leading indentation of each line.
    - `>"..."` trims leading and trailing whitespace for each line, and
      folds all lines into a single line, separated by a space.
    - ` ```...``` ` trims nothing, and supports unescaped double-quote and
      single-quote characters, and does not interpret any escape sequences
      (verbatim notation, like Markdown).
- Comments are preserved in the syntax tree, and have logical attachment to
  nodes. For example, a key-value pair preceded by a comment on a separate line,
  or followed by a comment on the same line, will be annotated such that the
  comment is available after parsing the document.

## `siml-fmt`

Since styling properties (like string folding options, sequence styles, etc.)
and comments are preserved by the parser, and can be used by the emitter. Thus,
the implementation of `siml-fmt` first parses the document, and then simply
emits the document again.

[^1]: The `?` prefix for complex keys significantly simplifies the parser by
    eliminating the need for left-recursion.
