use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use eyre::Result;
use split_point::SplitModuleIdentifier;

mod dep_graph;
mod emit;
mod range_map;
mod read;
mod reloc;
mod split_point;
mod util;

pub struct Options<'a> {
    /// The input wasm to split
    pub input_wasm: &'a [u8],
    /// Where to put javascript wrappers, split wasm modules
    pub output_dir: &'a Path,
    /// Where to put the main module that has to be post-processed by wasm-bindgen.
    /// usually a path in `output_dir`
    pub main_out_path: &'a Path,
    /// From where will `initSync` be imported from?
    pub main_module: &'a str,
    /// Verbosely output additional information about processing
    pub verbose: bool,
}

pub struct SplitWasm {
    pub split_modules: Vec<PathBuf>,
}

pub fn transform(opts: Options) -> Result<SplitWasm> {
    let module = crate::read::InputModule::parse(opts.input_wasm)?;
    if opts.verbose {
        module.reloc_info.print_relocs();
    }

    let dep_graph = dep_graph::get_dependencies(&module)?;
    let split_points = split_point::get_split_points(&module)?;
    let split_program_info =
        split_point::compute_split_modules(&module, &dep_graph, &split_points)?;

    if opts.verbose {
        for (name, split_deps) in split_program_info.output_modules.iter() {
            split_deps.print(format!("{:?}", name).as_str(), &module);
        }
    }

    let mut split_modules = vec![];
    let emit_fn = |output_module_index: usize, data: &[u8]| -> Result<()> {
        let identifier = &split_program_info.output_modules[output_module_index].0;
        let output_path = match identifier {
            SplitModuleIdentifier::Main => opts.main_out_path.to_path_buf(),
            _ => opts
                .output_dir
                .join(identifier.filename(output_module_index) + ".wasm"),
        };
        if !matches!(identifier, SplitModuleIdentifier::Main) {
            split_modules.push(output_path.clone());
        }
        std::fs::write(output_path, data)?;
        Ok(())
    };
    std::fs::create_dir_all(opts.output_dir)?;
    crate::emit::emit_modules(&module, &split_program_info, emit_fn)?;

    use std::fmt::Write;
    let mut javascript = String::new();
    let _ = write!(
        javascript,
        r#"import {{ initSync }} from "{}";
        "#,
        opts.main_module
    );
    javascript.push_str(include_str!("./split_wasm.js"));
    let mut split_deps = HashMap::<String, Vec<String>>::new();
    for (module_index, (name, _)) in split_program_info.output_modules.iter().enumerate() {
        let SplitModuleIdentifier::Chunk(splits) = name else {
            continue;
        };
        let file_name = name.filename(module_index);
        let var_name = format!("__chunk_{module_index}");
        for split in splits {
            split_deps
                .entry(split.clone())
                .or_default()
                .push(var_name.clone());
        }
        let splits = splits.join(", ");
        javascript.push_str(
            format!(
                "/* {splits} */\nconst {var_name} = makeLoad(new URL(\"./{file_name}.wasm\", import.meta.url), []);\n",
            )
            .as_str(),
        )
    }
    for (module_index, (identifier, _)) in
        split_program_info.output_modules.iter().enumerate().rev()
    {
        let split = match &identifier {
            SplitModuleIdentifier::Main | SplitModuleIdentifier::Chunk(_) => continue,
            SplitModuleIdentifier::Split(split) => split,
        };
        let file_name = identifier.filename(module_index);
        let loader_name = identifier.loader_name();
        let deps = split_deps.remove(split).unwrap_or_default();
        let deps = deps.join(", ");
        javascript.push_str(format!(
            "export const {loader_name} = wrapAsyncCb(makeLoad(new URL(\"./{file_name}.wasm\", import.meta.url), [{deps}]));\n",
        ).as_str())
    }

    std::fs::write(opts.output_dir.join("__wasm_split.js"), javascript)?;
    Ok(SplitWasm { split_modules })
}
