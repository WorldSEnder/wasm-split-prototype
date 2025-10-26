use eyre::Result;
use std::{collections::HashMap, fmt::Write, path::Path};

use crate::{
    dep_graph::DepNode,
    emit::EmitState,
    split_point::{OutputModuleInfo, SplitModuleIdentifier, SplitProgramInfo},
};

type PrefetchMap = HashMap<String, Vec<String>>;
pub struct LinkModuleWriter {
    javascript: String,
    prefetch_map: PrefetchMap,
}

impl LinkModuleWriter {
    fn new() -> Self {
        Self {
            javascript: String::new(),
            prefetch_map: HashMap::new(),
        }
    }
    fn write_main_import(&mut self, mod_path: &str) -> Result<()> {
        Ok(write!(
            &mut self.javascript,
            r#"import {{ initSync }} from "{}";
"#,
            mod_path
        )?)
    }
    fn write_get_shared_imports(&mut self, main_shares: &str) -> Result<()> {
        Ok(write!(
            &mut self.javascript,
            r#"let sharedImports = undefined;
function getSharedImports() {{
    if (sharedImports === undefined) {{
        sharedImports = {{ __wasm_split: {{ }} }};
        const mainExports = initSync(undefined, undefined);
        const {{ {main_shares} }} = mainExports;
        Object.assign(sharedImports.__wasm_split, {{ {main_shares} }});
    }}
    return sharedImports;
}}
"#
        )?)
    }
    fn write_runtime(&mut self) -> Result<()> {
        self.javascript
            .push_str(include_str!("./snippets/split_wasm.js"));
        self.javascript
            .push_str(include_str!("./snippets/makeFetch.web.js"));
        Ok(())
    }
    fn write_export_const(&mut self, name: &str, def: &impl std::fmt::Display) -> Result<()> {
        Ok(write!(
            &mut self.javascript,
            "export const {name} = {def};\n"
        )?)
    }
    fn fetch_opts<'pth>(&self, file_path: impl 'pth + std::fmt::Display) -> String {
        // Note: the expression returned from here should:
        // - allow lazily fetching the wasm module (no top-level import)
        // - allow bundlers and downstream code to recognize it as an expression to a path ("relocate" the import)
        // TODO: try other syntax for different targets:
        // - `import.source(<file_path>)`
        // - `URL.resolve(<file_path)` (support is not as good as for new URL)
        // Note: we use the form `new URL(<string literal>, import.meta.url)` which is understood by some
        // bundlers as syntax that can get rewritten if the path from where the file gets fetched is changed
        // (for example due to attaching a has of its contents).
        format!("new URL({}, import.meta.url)", file_path)
    }
    fn write_loaders(&mut self, program: &SplitProgramInfo) -> Result<()> {
        let mut split_deps = HashMap::<String, Vec<String>>::new();
        for (module_index, (name, _)) in program.output_modules.iter().enumerate() {
            let SplitModuleIdentifier::Chunk(splits) = name else {
                continue;
            };
            let file_name = name.filename(module_index);
            let var_name = format!("__chunk_{module_index}");
            let splits_dbg = splits.join(", ");
            write!(&mut self.javascript, "/* {splits_dbg} */\n")?;
            let fetch_opts = self.fetch_opts(format_args!("\"./{file_name}.wasm\""));
            write!(
                &mut self.javascript,
                "const {var_name} = makeLoad({fetch_opts}, []);\n"
            )?;
            for split in splits {
                split_deps
                    .entry(split.clone())
                    .or_default()
                    .push(var_name.clone());
                self.prefetch_map
                    .entry(split.clone())
                    .or_default()
                    .push(file_name.clone());
            }
        }
        for (module_index, (identifier, _)) in program.output_modules.iter().enumerate().rev() {
            let split = match &identifier {
                SplitModuleIdentifier::Main | SplitModuleIdentifier::Chunk(_) => continue,
                SplitModuleIdentifier::Split(split) => split,
            };
            let file_name = identifier.filename(module_index);
            let loader_name = identifier.loader_name();
            let deps = split_deps.remove(split).unwrap_or_default();
            let deps = deps.join(", ");
            let fetch_opts = self.fetch_opts(format_args!("\"./{file_name}.wasm\""));
            self.write_export_const(
                &loader_name,
                &format_args!("wrapAsyncCb(makeLoad({fetch_opts}, [{deps}]))"),
            )?;
            self.prefetch_map
                .entry(split.clone())
                .or_default()
                .push(file_name);
        }
        Ok(())
    }

    pub fn emit(self, path: &Path) -> Result<PrefetchMap> {
        std::fs::write(path, self.javascript)?;
        Ok(self.prefetch_map)
    }
}

fn reexported_shared_symbols(
    emit_state: &EmitState,
    program_info: &SplitProgramInfo,
    module: &OutputModuleInfo,
) -> Result<String> {
    let mut shares = String::new();
    let exported = program_info.shared_deps.iter().filter_map(|dep| {
        if let DepNode::Function(_) | DepNode::DataSymbol(_) = dep {
            return None;
        }
        if !module.included_symbols.contains(dep) {
            return None;
        }
        Some(emit_state.name_for(dep))
    });
    for export in exported {
        let () = write!(&mut shares, "{}, ", export.as_ref())?;
    }
    Ok(shares)
}

pub fn link_module(
    main_module_path: &str,
    program_info: &SplitProgramInfo,
    emit_state: &EmitState,
) -> Result<LinkModuleWriter> {
    let mut link_module = LinkModuleWriter::new();

    let (_, main_module) = program_info
        .output_modules
        .iter()
        .find(|(id, _)| matches!(id, SplitModuleIdentifier::Main))
        .unwrap();
    let main_shared = reexported_shared_symbols(emit_state, program_info, main_module)?;

    link_module.write_main_import(main_module_path)?;
    link_module.write_get_shared_imports(&main_shared)?;
    link_module.write_runtime()?;
    link_module.write_loaders(program_info)?;
    Ok(link_module)
}
