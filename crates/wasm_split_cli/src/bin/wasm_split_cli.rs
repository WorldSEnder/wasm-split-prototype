use clap::Parser;
use eyre::Result;
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
    tracing_subscriber::fmt::init();

    let args = Cli::parse();
    let input_wasm = std::fs::read(args.input)?;
    let main_out_path = args.output.join("main.wasm");
    let _ = this::transform({
        let mut opts = this::Options::new(&input_wasm);
        opts.verbose = args.verbose;
        opts.output_dir = &args.output;
        opts.main_out_path = &main_out_path;
        opts
    })?;
    Ok(())
}
