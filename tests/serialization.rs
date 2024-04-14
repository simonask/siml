#![cfg(feature = "serde")]

use siml::{
    location::{SourceLocation, Span, Spanned},
    *,
};

#[derive(serde::Serialize, serde::Deserialize, PartialEq, Debug)]
struct Basic {
    foo: i32,
    bar: String,
}

#[test]
fn serialize_basic() {
    let value = Basic {
        foo: 42,
        bar: "Hello, World!".into(),
    };
    assert_eq!(
        to_document(&value).unwrap(),
        Value::mapping([
            (Some(Value::plain("foo")), Value::plain("42")),
            (
                Some(Value::plain("bar")),
                Value::quoted("Hello, World!".into())
            ),
        ])
    );
    assert_eq!(
        to_string(&value).unwrap(),
        "foo: 42\nbar: \"Hello, World!\""
    );
}

#[derive(serde::Serialize, serde::Deserialize, PartialEq, Debug)]
enum Enum {
    Unit,
    Newtype(Basic),
    Struct { foo: i32, bar: Option<String> },
}

#[derive(serde::Serialize, serde::Deserialize, PartialEq, Debug)]
struct Nested {
    basic: Enum,
    baz: Vec<Nested>,
}

#[test]
fn complex_serialization() {
    let value = Nested {
        basic: Enum::Struct {
            foo: 42,
            bar: Some("Hello, World!".into()),
        },
        baz: vec![
            Nested {
                basic: Enum::Unit,
                baz: vec![],
            },
            Nested {
                basic: Enum::Newtype(Basic {
                    foo: 42,
                    bar: "Hello, World! Long string".into(),
                }),
                baz: vec![],
            },
        ],
    };
    assert_eq!(
        to_string(&value).unwrap(),
        r#"basic: {
  foo: 42
  bar: Some("Hello, World!")
}
baz: [
  {
    basic: Unit
    baz: []
  },
  {
    basic: Newtype({
      foo: 42
      bar: "Hello, World! Long string"
    })
    baz: []
  },
]"#
    );
}

#[test]
fn deserialize_span() {
    #[derive(serde::Serialize, serde::Deserialize)]
    struct Root {
        values: Vec<Spanned<Inner>>,
    }
    #[derive(serde::Serialize, serde::Deserialize)]
    struct Inner {
        value: i32,
    }

    let input: &str = r#"
values: [
    { value: 42 },
    { value: 43 }
]
"#;
    let root: Root = from_string(input).unwrap();
    assert_eq!(root.values.len(), 2);
    assert_eq!(root.values[0].value.value, 42);
    assert_eq!(root.values[1].value.value, 43);
    assert_eq!(
        root.values[0].span,
        Span {
            start: SourceLocation {
                offset: 15,
                line: 2,
                column: 4
            },
            end: SourceLocation {
                offset: 28,
                line: 2,
                column: 17
            },
        }
    );
    assert_eq!(
        root.values[1].span,
        Span {
            start: SourceLocation {
                offset: 34,
                line: 3,
                column: 4
            },
            end: SourceLocation {
                offset: 47,
                line: 3,
                column: 17
            },
        }
    );
}
