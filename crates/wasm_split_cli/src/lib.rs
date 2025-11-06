use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use eyre::{Result, WrapErr};
use split_point::SplitModuleIdentifier;

mod dep_graph;
mod emit;
mod graph_utils;
mod js;
mod magic_constants;
mod read;
mod reloc;
mod split_point;
mod util;

#[non_exhaustive]
pub struct Options<'a> {
    /// The input wasm to split
    pub input_wasm: &'a [u8],
    /// Where to put javascript wrappers, split wasm modules.
    ///
    /// Default: `Path::new("wasm_split")`
    pub output_dir: &'a Path,
    /// Where to put the main module that has to be post-processed by wasm-bindgen.
    /// Usually a path in `output_dir`.
    ///
    /// Default: `Path::new("wasm_split/main.wasm")`
    pub main_out_path: &'a Path,
    /// Module path of the created link file, relative to the output dir.
    /// The wasm will use this path to import the loader functions for the split chunks.
    ///
    /// Default: `"./__wasm_split.js"`
    pub link_name: &'a str,
    /// From where will `initSync` be imported from?
    ///
    /// Default: `"./main.js"`
    pub main_module: &'a str,
    /// Verbosely output additional information about processing.
    ///
    /// Default: false
    pub verbose: bool,
    /// Enables explicit tests for assumptions we make about the input wasm file during integration testing.
    #[doc(hidden)]
    pub strict_tests: bool,
}

impl<'wasm> Options<'wasm> {
    pub fn new(input_wasm: &'wasm [u8]) -> Self {
        Self {
            input_wasm,
            output_dir: Path::new("wasm_split"),
            main_out_path: Path::new("wasm_split/main.wasm"),
            link_name: "./__wasm_split.js",
            main_module: "./main.js",
            verbose: false,
            strict_tests: false,
        }
    }
}

#[non_exhaustive]
pub struct SplitWasm {
    pub split_modules: Vec<PathBuf>,
    /// split -> dependency filestem
    /// e.g. `{ "foo": ["chunk_0", "foo"] }`
    pub prefetch_map: HashMap<String, Vec<String>>,
}

pub fn transform(opts: Options) -> Result<SplitWasm> {
    let strictness = if opts.strict_tests {
        read::Strictness::IntegrationTesting
    } else {
        read::Strictness::Lenient
    };
    // (1) parse input
    let module = crate::read::InputModule::parse(opts.input_wasm, strictness)?;
    if opts.verbose {
        module.reloc_info.print_relocs();
    }
    // (2) dependency analysis and decide on splits
    let dep_graph = dep_graph::get_dependencies(&module)?;
    let split_points = split_point::get_split_points(&module)?;
    let split_program_info =
        split_point::compute_split_modules(&module, &dep_graph, &split_points)?;

    if split_point::trace_enabled(opts.verbose) {
        for (name, split_deps) in split_program_info.output_modules.iter() {
            split_deps.print(format!("{:?}", name).as_str(), &module);
        }
    }
    // (3) compute output modules and helper javascript
    let link_module = opts.link_name;
    let emit_state = emit::EmitState::new(&opts, &module, &split_program_info, link_module)?;
    let wasm_modules = emit::emit_modules(
        &split_program_info,
        &emit_state,
        |output_module_index, identifier, data| {
            let output_path = match identifier {
                SplitModuleIdentifier::Main => opts.main_out_path.to_path_buf(),
                _ => opts
                    .output_dir
                    .join(identifier.filename(output_module_index) + ".wasm"),
            };
            (identifier, output_path, data)
        },
    )?;
    let js_link_module = js::link_module(opts.main_module, &split_program_info, &emit_state)?;
    // (4) write the output
    std::fs::create_dir_all(opts.output_dir)?;
    let mut split_modules = vec![];
    for (identifier, output_path, data) in wasm_modules {
        // TODO: we could do this asynchronously
        std::fs::write(&output_path, &data)
            .with_context(|| format!("Error emitting {:?}", identifier))?;
        if !matches!(identifier, SplitModuleIdentifier::Main) {
            split_modules.push(output_path);
        }
    }
    let prefetch_map = js_link_module.emit(&opts.output_dir.join(Path::new(link_module)))?;

    Ok(SplitWasm {
        split_modules,
        prefetch_map,
    })
}
