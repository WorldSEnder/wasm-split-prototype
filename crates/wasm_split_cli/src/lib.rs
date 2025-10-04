use std::{collections::HashMap, path::Path};

use anyhow::Result;
use split_point::SplitModuleIdentifier;

mod dep_graph;
mod emit;
mod range_map;
mod read;
mod reloc;
mod split_point;
mod util;

pub struct Options<'a> {
    pub input: &'a Path,
    pub output: &'a Path,
    pub verbose: bool,
}

pub fn transform(args: Options) -> Result<()> {
    let input_wasm = std::fs::read(args.input)?;
    let module = crate::read::InputModule::parse(&input_wasm)?;
    if args.verbose {
        module.reloc_info.print_relocs();
    }

    let dep_graph = dep_graph::get_dependencies(&module)?;
    let split_points = split_point::get_split_points(&module)?;
    let split_program_info =
        split_point::compute_split_modules(&module, &dep_graph, &split_points)?;

    if args.verbose {
        for (name, split_deps) in split_program_info.output_modules.iter() {
            split_deps.print(format!("{:?}", name).as_str(), &module);
        }
    }

    let emit_fn = |output_module_index: usize, data: &[u8]| -> Result<()> {
        let identifier = &split_program_info.output_modules[output_module_index].0;
        let output_filename = identifier.filename(output_module_index) + ".wasm";
        let output_path = args.output.join(output_filename);
        std::fs::create_dir_all(args.output)?;
        std::fs::write(output_path, data)?;
        Ok(())
    };
    crate::emit::emit_modules(&module, &split_program_info, emit_fn)?;

    let mut javascript = String::new();
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

    std::fs::write(args.output.join("__wasm_split.js"), javascript)?;
    Ok(())
}
