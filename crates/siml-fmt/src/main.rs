use clap::{parser, Parser};
use siml::{Document, Emitter};

#[derive(clap::Parser, Debug)]
struct Args {
    #[clap(value_parser, default_value = "-")]
    input: clio::Input,
    #[clap(value_parser, default_value = "-")]
    output: clio::Output,
}

fn main() {
    let mut args = Args::parse();
    let mut input = args.input.lock();

    let mut parser = siml::ParseStream::new(&mut *input);
    let doc = match Document::from_parser(&mut parser) {
        Ok(doc) => doc,
        Err(err) => {
            eprintln!("Parsing error: {err}");
            std::process::exit(1);
        }
    };

    let mut output = args.output.lock();
    let emitter = Emitter::with_io_writer(&mut *output);
    match emitter.emit(&doc) {
        Ok(()) => {}
        Err(err) => {
            std::mem::drop(output);
            eprintln!("Emitting error: {err}");
            std::process::exit(1);
        }
    }
}
