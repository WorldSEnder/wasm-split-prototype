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
mod options;
mod read;
mod reloc;
mod split_point;
mod tracing_support;
mod util;

pub use options::*;

use tracing_support::perf_span;

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
    let parse_span = perf_span!("parse");
    let parse_span = parse_span.enter();
    let module = crate::read::InputModule::parse(opts.input_wasm, strictness)?;
    parse_span.exit();
    if opts.verbose {
        module.reloc_info.print_relocs();
    }
    // (2) dependency analysis and decide on splits
    let deps_span = perf_span!("dependency-analysis");
    let deps_span = deps_span.enter();
    let deps = dep_graph::get_dependencies(&module)?;
    let split_points = split_point::get_split_points(&module)?;
    let split_program_info =
        split_point::compute_split_modules(&module, &deps.graph, split_points)?;
    deps_span.exit();

    if split_point::trace_enabled(opts.verbose) {
        for (name, split_deps) in split_program_info.output_modules.iter() {
            split_deps.print(format!("{:?}", name).as_str(), &module);
        }
    }
    // (3) compute output modules and helper javascript
    let emit_span = perf_span!("emit");
    let emit_span = emit_span.enter();
    let link_module = opts.link_name;
    let emit_state = emit::EmitState::new(
        &opts,
        &module,
        &split_program_info,
        link_module,
        &deps.stub_fns,
    )?;
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
    emit_span.exit();
    // (4) write the output
    let write_span = perf_span!("write");
    let write_span = write_span.enter();
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
    write_span.exit();

    Ok(SplitWasm {
        split_modules,
        prefetch_map,
    })
}
