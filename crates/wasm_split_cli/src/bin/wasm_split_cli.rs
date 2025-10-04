use anyhow::Result;
use clap::Parser;
use std::path::Path;

use wasm_split_cli_support as this;

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
    let input_wasm = std::fs::read(args.input)?;
    let main_out_path = args.output.join("main.wasm");
    let opts = this::Options {
        input_wasm: &input_wasm,
        output_dir: &args.output,
        main_out_path: &main_out_path,
        verbose: args.verbose,
        main_module: "./main.js",
    };
    this::transform(opts)
}
