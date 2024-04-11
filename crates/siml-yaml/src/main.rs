use clap::Parser;

#[derive(clap::Parser, Debug)]
struct Args {
    #[clap(value_parser, short, long, default_value = "-")]
    input: clio::Input,
    #[clap(value_parser, short, long, default_value = "-")]
    output: clio::Output,
    #[clap(value_parser, long)]
    input_format: Option<Format>,
    #[clap(value_parser, long)]
    output_format: Option<Format>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, clap::ValueEnum)]
enum Format {
    #[clap(alias = "yaml")]
    Yaml,
    #[clap(alias = "siml")]
    Siml,
}

#[derive(Debug)]
enum Error {
    Siml(siml::Error),
    Yaml(libyaml_safer::Error),
    Write(std::fmt::Error),
}

impl From<siml::Error> for Error {
    fn from(err: siml::Error) -> Self {
        Error::Siml(err)
    }
}

impl From<libyaml_safer::Error> for Error {
    fn from(err: libyaml_safer::Error) -> Self {
        Error::Yaml(err)
    }
}

impl From<std::fmt::Error> for Error {
    fn from(value: std::fmt::Error) -> Self {
        Error::Write(value)
    }
}

impl From<siml::BuilderError> for Error {
    fn from(value: siml::BuilderError) -> Self {
        Error::Siml(value.into())
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Siml(err) => write!(f, "SIML error: {err}"),
            Error::Yaml(err) => write!(f, "YAML error: {err}"),
            Error::Write(err) => write!(f, "Write error: {err}"),
        }
    }
}

fn main() {
    let mut args = Args::parse();
    let input_format = if let Some(input_format) = args.input_format {
        input_format
    } else {
        if args.input.is_std() {
            eprintln!("--input-format is required when reading from stdin");
            std::process::abort()
        } else {
            match args.input.path().extension().and_then(|ext| ext.to_str()) {
                Some("yaml" | "yml") => Format::Yaml,
                Some("siml") => Format::Siml,
                Some(ext) => {
                    eprintln!(
                        "Unknown input file extension, and `--input-format` not given: {ext}"
                    );
                    std::process::abort()
                }
                None => {
                    eprintln!(
                        "--input-format is required when reading from a file without an extension"
                    );
                    std::process::abort()
                }
            }
        }
    };
    let output_format = if let Some(output_format) = args.output_format {
        output_format
    } else {
        if args.output.is_std() {
            if input_format == Format::Yaml {
                Format::Siml
            } else {
                Format::Yaml
            }
        } else {
            match args.output.path().extension().and_then(|ext| ext.to_str()) {
                Some("yaml" | "yml") => Format::Yaml,
                Some("siml") => Format::Siml,
                Some(ext) => {
                    eprintln!(
                        "Unknown output file extension, and `--output-format` not given: {ext}"
                    );
                    std::process::abort()
                }
                None => {
                    eprintln!(
                        "--output-format is required when writing to a file without an extension"
                    );
                    std::process::abort()
                }
            }
        }
    };

    let result = match (input_format, output_format) {
        (Format::Yaml, Format::Siml) => yaml_to_siml(&mut args.input, &mut args.output),
        (Format::Siml, Format::Yaml) => siml_to_yaml(&mut args.input, &mut args.output),
        _ => {
            eprintln!("Input and output formats must be different");
            std::process::abort()
        }
    };

    if let Err(e) = result {
        eprintln!("{e}");
    }
}

fn yaml_to_siml(
    input: &mut dyn std::io::Read,
    output: &mut dyn std::io::Write,
) -> Result<(), Error> {
    let mut reader = std::io::BufReader::new(input);
    let mut parser = libyaml_safer::Parser::new();
    parser.set_input(&mut reader);
    let mut in_doc = libyaml_safer::Document::load(&mut parser)?;
    let mut out_doc = siml::Document::default();

    fn add_value_to_doc(
        in_doc: &mut libyaml_safer::Document,
        in_node: i32,
        out_doc: &mut siml::Document,
    ) -> Result<siml::NodeId, Error> {
        let in_node = in_doc.get_node(in_node).expect("invalid input node");
        match &in_node.data {
            libyaml_safer::NodeData::NoNode => out_doc
                .add_scalar(siml::Scalar::plain(""))
                .map_err(Into::into),
            libyaml_safer::NodeData::Scalar { value, style } => {
                add_scalar_to_doc(value, in_node.tag.as_deref(), *style, out_doc)
            }
            libyaml_safer::NodeData::Sequence { items, style } => {
                let items = items.clone(); // TODO: Missing non-mutable get_node()
                let style = *style;
                let in_type_tag = in_node.tag.clone();
                add_sequence_to_doc(in_doc, in_type_tag.as_deref(), &*items, style, out_doc)
            }
            libyaml_safer::NodeData::Mapping { pairs, style } => {
                let pairs = pairs.clone(); // TODO: Missing non-mutable get_node()
                let style = *style;
                let in_type_tag = in_node.tag.clone();
                add_mapping_to_doc(in_doc, in_type_tag.as_deref(), &*pairs, style, out_doc)
            }
        }
    }

    fn add_scalar_to_doc(
        value: &str,
        in_type_tag: Option<&str>,
        style: libyaml_safer::ScalarStyle,
        out_doc: &mut siml::Document,
    ) -> Result<siml::NodeId, Error> {
        let id = out_doc.add_scalar(siml::Scalar {
            value,
            style: match style {
                libyaml_safer::ScalarStyle::Plain | libyaml_safer::ScalarStyle::Literal => {
                    siml::ScalarStyle::Plain
                }
                libyaml_safer::ScalarStyle::SingleQuoted => siml::ScalarStyle::SingleQuoted,
                libyaml_safer::ScalarStyle::DoubleQuoted => siml::ScalarStyle::Quoted,
                libyaml_safer::ScalarStyle::Folded => siml::ScalarStyle::QuotedTrimIndent,
                _ => siml::ScalarStyle::Quoted,
            },
            anchor: None,
        })?;

        let in_type_tag_node = in_type_tag
            .map(|s| out_doc.add_scalar(siml::Scalar::plain(s.strip_prefix('!').unwrap_or(s))))
            .transpose()?;
        if let Some(in_type_tag) = in_type_tag {
            if in_type_tag != "tag:yaml.org,2002:str" && in_type_tag != "tag:yaml.org,2002:number" {
                let tuple =
                    out_doc.add_sequence(siml::SequenceStyle::Tuple, None, in_type_tag_node)?;
                out_doc.add_sequence_item(Some(tuple), None, id);
                out_doc.complete_sequence(tuple);
                return Ok(tuple);
            }
        }

        Ok(id)
    }

    fn add_sequence_to_doc(
        in_doc: &mut libyaml_safer::Document,
        in_type_tag: Option<&str>,
        in_items: &[i32],
        _style: libyaml_safer::SequenceStyle,
        out_doc: &mut siml::Document,
    ) -> Result<siml::NodeId, Error> {
        let out_type_tag = in_type_tag
            .filter(|s| *s != "tag:yaml.org,2002:seq")
            .map(|s| out_doc.add_scalar(siml::Scalar::plain(s)))
            .transpose()?;
        let out_seq = out_doc.add_sequence(siml::SequenceStyle::List, None, out_type_tag)?;
        for in_node in in_items.iter().copied() {
            let v = add_value_to_doc(in_doc, in_node, out_doc)?;
            out_doc.add_sequence_item(Some(out_seq), None, v);
        }
        out_doc.complete_sequence(out_seq);
        Ok(out_seq)
    }

    fn add_mapping_to_doc(
        in_doc: &mut libyaml_safer::Document,
        in_type_tag: Option<&str>,
        in_pairs: &[libyaml_safer::NodePair],
        _style: libyaml_safer::MappingStyle,
        out_doc: &mut siml::Document,
    ) -> Result<siml::NodeId, Error> {
        let out_type_tag = in_type_tag
            .filter(|s| *s != "tag:yaml.org,2002:map")
            .map(|s| out_doc.add_scalar(siml::Scalar::plain(s)))
            .transpose()?;
        let out_mapping = out_doc.add_sequence(siml::SequenceStyle::Mapping, None, out_type_tag)?;
        copy_into_mapping(in_doc, in_pairs, out_doc, Some(out_mapping))?;
        out_doc.complete_sequence(out_mapping);
        Ok(out_mapping)
    }

    fn copy_into_mapping(
        in_doc: &mut libyaml_safer::Document,
        in_pairs: &[libyaml_safer::NodePair],
        out_doc: &mut siml::Document,
        out_mapping: Option<siml::NodeId>,
    ) -> Result<(), Error> {
        for pair in in_pairs {
            let k = add_value_to_doc(in_doc, pair.key, out_doc)?;
            let v = add_value_to_doc(in_doc, pair.value, out_doc)?;
            out_doc.add_sequence_item(out_mapping, Some(k), v);
        }
        Ok(())
    }

    if let Some(in_root) = in_doc.get_root_node() {
        // If the root node is not a mapping, insert it into the implicit root
        // mapping with an empty key.
        match &in_root.data {
            libyaml_safer::NodeData::NoNode => (),
            libyaml_safer::NodeData::Scalar { value, style } => {
                let single =
                    add_scalar_to_doc(&*value, in_root.tag.as_deref(), *style, &mut out_doc)?;
                out_doc.add_sequence_item(None, None, single);
            }
            libyaml_safer::NodeData::Sequence { items, style } => {
                let items = items.clone(); // TODO: Missing non-mutable get_node()
                let style = *style;
                let in_type_tag = in_root.tag.clone();
                let single = add_sequence_to_doc(
                    &mut in_doc,
                    in_type_tag.as_deref(),
                    &*items,
                    style,
                    &mut out_doc,
                )?;
                out_doc.add_sequence_item(None, None, single);
            }
            libyaml_safer::NodeData::Mapping { pairs, style: _ } => {
                let pairs = pairs.clone(); // TODO: Missing non-mutable get_node()
                copy_into_mapping(&mut in_doc, &*pairs, &mut out_doc, None)?;
            }
        }
    }

    let emitter = siml::Emitter::with_io_writer(output);
    emitter.emit(&out_doc)?;
    Ok(())
}

fn siml_to_yaml(
    input: &mut dyn std::io::Read,
    output: &mut dyn std::io::Write,
) -> Result<(), Error> {
    let reader = std::io::BufReader::new(input);
    let mut parser = siml::ParseStream::new(reader);
    let in_doc = siml::Document::from_parser(&mut parser)?;
    let mut out_doc = libyaml_safer::Document::new(None, &[], true, true);

    fn add_node_to_doc(
        in_doc: &siml::Document,
        in_node: siml::NodeId,
        in_tag: Option<&str>,
        out_doc: &mut libyaml_safer::Document,
    ) -> Result<i32, Error> {
        let node = in_doc.get_node(in_node);
        match node.data {
            siml::private::NodeData::Scalar(ref s) => {
                let style = match s.style {
                    siml::ScalarStyle::Plain => libyaml_safer::ScalarStyle::Plain,
                    siml::ScalarStyle::SingleQuoted => libyaml_safer::ScalarStyle::SingleQuoted,
                    siml::ScalarStyle::Quoted | siml::ScalarStyle::QuotedTrimWhitespace => {
                        libyaml_safer::ScalarStyle::DoubleQuoted
                    }
                    siml::ScalarStyle::QuotedTrimIndent => libyaml_safer::ScalarStyle::DoubleQuoted,
                };
                Ok(out_doc.add_scalar(in_tag, &s.value, style))
            }
            siml::private::NodeData::Sequence(ref in_seq) => {
                let type_tag = if let Some(type_tag_node) = in_seq.type_tag {
                    let type_tag_node = in_doc.get_node(type_tag_node);
                    if let siml::private::NodeData::Scalar(ref s) = type_tag_node.data {
                        Some(s.value.as_str())
                    } else {
                        None
                    }
                } else {
                    None
                };
                if in_seq.style == siml::SequenceStyle::Mapping {
                    let mapping = out_doc.add_mapping(type_tag, libyaml_safer::MappingStyle::Block);
                    copy_into_mapping(in_doc, in_seq, out_doc, mapping)?;
                    return Ok(mapping);
                } else if in_seq.style == siml::SequenceStyle::Tuple
                    && in_seq.items.len() == 1
                    && in_tag.is_none()
                    && type_tag.is_some()
                {
                    let v = add_node_to_doc(in_doc, in_seq.items[0].value_node, type_tag, out_doc)?;
                    return Ok(v);
                }

                let seq = out_doc.add_sequence(type_tag, libyaml_safer::SequenceStyle::Block);
                copy_into_sequence(in_doc, in_seq, out_doc, seq)?;
                Ok(seq)
            }
        }
    }

    fn copy_into_sequence(
        in_doc: &siml::Document,
        in_seq: &siml::private::SequenceNode,
        out_doc: &mut libyaml_safer::Document,
        out_seq: i32,
    ) -> Result<(), Error> {
        for item in in_seq.items.iter() {
            let v = add_node_to_doc(in_doc, item.value_node, None, out_doc)?;
            out_doc.append_sequence_item(out_seq, v);
        }
        Ok(())
    }

    fn copy_into_mapping(
        in_doc: &siml::Document,
        in_seq: &siml::private::SequenceNode,
        out_doc: &mut libyaml_safer::Document,
        out_mapping: i32,
    ) -> Result<(), Error> {
        for item in in_seq.items.iter() {
            let k = if let Some(key_node) = item.key_node {
                add_node_to_doc(in_doc, key_node, None, out_doc)?
            } else {
                out_doc.add_scalar(None, "~", libyaml_safer::ScalarStyle::Plain)
            };
            let v = add_node_to_doc(in_doc, item.value_node, None, out_doc)?;
            out_doc.yaml_document_append_mapping_pair(out_mapping, k, v);
        }
        Ok(())
    }

    let in_root = in_doc.get_root();
    if in_root.style == siml::SequenceStyle::Mapping
        && in_root.items.len() == 1
        && in_root.items[0].key_node.is_none()
    {
        // Skip the root mapping if it is a single item with no key.
        let out_root = add_node_to_doc(&in_doc, in_root.items[0].value_node, None, &mut out_doc)?;
        assert_eq!(out_root, 1);
    } else {
        let out_root = out_doc.add_mapping(None, libyaml_safer::MappingStyle::Block);
        copy_into_mapping(&in_doc, in_root, &mut out_doc, out_root)?;
    }

    let mut emitter = libyaml_safer::Emitter::new();
    emitter.set_output(output);
    emitter.set_unicode(true);
    emitter.set_encoding(libyaml_safer::Encoding::Utf8);
    emitter.set_width(100);
    out_doc.dump(&mut emitter)?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn single_value_roundtrip() {
        let input = "foo\n";
        let expected = "foo";
        let expected_yaml = "foo\n";
        let mut siml_out = Vec::new();
        let mut yaml_out = Vec::new();

        let mut yaml_in = input.as_bytes();
        yaml_to_siml(&mut yaml_in, &mut siml_out).unwrap();

        let mut siml_in = input.as_bytes();
        siml_to_yaml(&mut siml_in, &mut yaml_out).unwrap();

        assert_eq!(std::str::from_utf8(&siml_out).unwrap(), expected);
        assert_eq!(std::str::from_utf8(&yaml_out).unwrap(), expected_yaml);
    }

    #[test]
    fn root_sequence_roundtrip() {
        let yaml_in = r#"- foo
- bar
- baz
"#;
        let siml_in = r#"[foo, bar, baz]"#;

        let mut siml_out = Vec::new();
        let mut yaml_out = Vec::new();

        yaml_to_siml(&mut yaml_in.as_bytes(), &mut siml_out).unwrap();
        siml_to_yaml(&mut siml_in.as_bytes(), &mut yaml_out).unwrap();

        assert_eq!(std::str::from_utf8(&siml_out).unwrap(), siml_in);
        assert_eq!(std::str::from_utf8(&yaml_out).unwrap(), yaml_in);
    }

    #[test]
    fn type_tags() {
        let yaml_in = "!MyKeyType foo: !MyValueType bar\n";
        let yaml_expected = "!<MyKeyType> foo: !<MyValueType> bar\n"; // The YAML emitter adds angle brackets to type tags.
        let siml_in = r#"? MyKeyType(foo): MyValueType(bar)"#;

        let mut siml_out = Vec::new();
        let mut yaml_out = Vec::new();

        yaml_to_siml(&mut yaml_in.as_bytes(), &mut siml_out).unwrap();
        siml_to_yaml(&mut siml_in.as_bytes(), &mut yaml_out).unwrap();

        assert_eq!(std::str::from_utf8(&siml_out).unwrap(), siml_in);
        assert_eq!(std::str::from_utf8(&yaml_out).unwrap(), yaml_expected);
    }
}
