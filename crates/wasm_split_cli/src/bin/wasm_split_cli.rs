use clap::{Parser, ValueEnum};
use eyre::Result;
use std::path::Path;

use wasm_split_cli_support as this;

#[derive(Debug, ValueEnum, Clone, Copy, PartialEq, Eq, Default)]
enum Target {
    #[default]
    Web,
    Bundler,
}

#[derive(Debug, Parser)]
#[command(name = "wasm-split")]
struct Cli {
    /// Input .wasm file.
    input: Box<Path>,

    /// Output directory.
    output: Box<Path>,

    /// Output target
    #[arg(long)]
    target: Option<Target>,

    /// Output module name
    #[arg(long)]
    out_name: Option<String>,

    /// Print verbose split information.
    #[arg(short, long)]
    verbose: bool,
}

fn main() -> Result<()> {
    tracing_subscriber::fmt::init();

    let args = Cli::parse();
    let input_wasm = std::fs::read(args.input)?;
    let stem = match args.out_name.as_ref() {
        Some(name) => name,
        None => "main",
    };
    let main_module;
    let main_out_path = args.output.join(&format!("{stem}.wasm"));
    let _ = this::transform({
        let mut opts = this::Options::new(&input_wasm);
        opts.verbose = args.verbose;
        opts.output_dir = &args.output;
        opts.main_out_path = &main_out_path;
        if let Some(name) = args.out_name.as_ref() {
            main_module = match args.target {
                None | Some(Target::Web) => format!("./{name}.js"),
                Some(Target::Bundler) => format!("./{name}_bg.wasm"),
            };
            opts.main_module = &main_module;
        }
        match args.target {
            Some(Target::Web) => {
                opts.target.web();
            }
            Some(Target::Bundler) => {
                opts.target.bundler();
            }
            None => {}
        }
        opts
    })?;
    Ok(())
}
