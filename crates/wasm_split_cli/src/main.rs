use anyhow::Result;
use clap::Parser;
use wasm_split_cli_support::{self as this, transform, Options};

#[derive(Debug, Parser)]
#[command(name = "wasm-split")]
struct Cli {
    /// Input .wasm file.
    input: Box<Path>,

    /// Output directory.
    output: Box<Path>,

    /// Print verbose split information.
    #[arg(short, long)]
    verbose: bool,
}

fn main() -> Result<()> {
    let args = Cli::parse();
    let opts = this::Options {
        input: &args.input,
        output: &args.output,
        verbose: args.verbose,
    };
    this::transform(opts)
}
