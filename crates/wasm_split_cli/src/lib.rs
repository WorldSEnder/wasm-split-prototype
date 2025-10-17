use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use eyre::Result;
use split_point::SplitModuleIdentifier;

mod dep_graph;
mod emit;
//mod range_map;
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
    /// Path of the created link file, relative to the output dir.
    /// The wasm will use this path to import the loader functions for the split chunks.
    /// This must match the `link_name` used in the macro.
    ///
    /// Default: `Path::new("./__wasm_split.js")`
    pub link_name: &'a Path,
    /// From where will `initSync` be imported from?
    ///
    /// Default: `"./main.js"`
    pub main_module: &'a str,
    /// Verbosely output additional information about processing.
    ///
    /// Default: false
    pub verbose: bool,
}

impl<'wasm> Options<'wasm> {
    pub fn new(input_wasm: &'wasm [u8]) -> Self {
        Self {
            input_wasm,
            output_dir: Path::new("wasm_split"),
            main_out_path: Path::new("wasm_split/main.wasm"),
            link_name: Path::new("./__wasm_split.js"),
            main_module: "./main.js",
            verbose: false,
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
    let mut prefetch_map = HashMap::<String, Vec<String>>::new();

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
        let splits_dbg = splits.join(", ");
        javascript.push_str(
            format!(
                "/* {splits_dbg} */\nconst {var_name} = makeLoad(new URL(\"./{file_name}.wasm\", import.meta.url), []);\n",
            )
            .as_str(),
        );
        for split in splits {
            split_deps
                .entry(split.clone())
                .or_default()
                .push(var_name.clone());
            prefetch_map
                .entry(split.clone())
                .or_default()
                .push(file_name.clone());
        }
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
        ).as_str());
        prefetch_map
            .entry(split.clone())
            .or_default()
            .push(file_name);
    }

    let link_path = opts.output_dir.join(opts.link_name);
    std::fs::write(link_path, javascript)?;
    Ok(SplitWasm {
        split_modules,
        prefetch_map,
    })
}
