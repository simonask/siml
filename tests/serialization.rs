#![cfg(feature = "serde")]

use siml::*;

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
