use clap::{Args, Parser, ValueEnum};
use eyre::Result;
use std::path::Path;

use wasm_split_cli_support as this;

#[derive(Debug, ValueEnum, Clone, Copy, PartialEq, Eq, Default)]
enum Target {
    #[default]
    Web,
    Bundler,
}

#[derive(Debug, ValueEnum, Clone, Copy, PartialEq, Eq, Default)]
enum CostModel {
    #[default]
    Auto,
    None,
    Binary,
}

#[derive(Debug, Args)]
#[command(next_help_heading = "Options for `--cost-model=binary`")]
struct CostModelBinaryOpts {
    /// What one more chunk request costs, in downloaded bytes. 0 keeps every chunk as
    /// split.
    #[arg(long, default_value_t = 16 * 1024)]
    request_cost: usize,

    /// The chance that one split module gets loaded.
    #[arg(long, default_value_t = 0.05)]
    load_probability: f64,
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

    /// The cost model under which the chunk count is reduced.
    /// Default: auto, which infers a cost model from the target.
    #[arg(long)]
    cost_model: Option<CostModel>,

    /// Print verbose split information.
    #[arg(short, long)]
    verbose: bool,

    #[clap(flatten)]
    cost_model_bin_opts: CostModelBinaryOpts,
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
        match args.cost_model.unwrap_or_default() {
            CostModel::Binary if args.cost_model_bin_opts.request_cost > 0 => {
                opts.cost_model = Some(this::CostModel::Binary(this::BernoulliCost {
                    request_cost: args.cost_model_bin_opts.request_cost,
                    load_probability: args.cost_model_bin_opts.load_probability,
                }))
            }
            CostModel::Binary | CostModel::None => {
                opts.cost_model = Some(this::CostModel::DisableMerging)
            }
            _ => {}
        }
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
