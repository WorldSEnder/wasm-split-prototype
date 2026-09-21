use eyre::Result;
use std::{collections::HashMap, fmt::Write, path::Path};

use crate::{
    dep_graph::DepNode,
    emit::EmitState,
    read::InputModule,
    split_point::{OutputModuleInfo, SplitModuleIdentifier, SplitProgramInfo},
    tracing_support::perf_span,
};

type PrefetchMap = HashMap<String, Vec<String>>;
pub struct LinkModuleWriter<'p> {
    input_module: &'p InputModule<'p>,
    input_options: &'p crate::Options<'p>,
    program_info: &'p SplitProgramInfo,
    javascript: String,
    prefetch_map: PrefetchMap,
}

impl<'p> LinkModuleWriter<'p> {
    fn new(program_info: &'p SplitProgramInfo, emit_state: &'p EmitState) -> Self {
        Self {
            program_info,
            input_module: emit_state.input(),
            input_options: emit_state.input_options(),
            javascript: String::new(),
            prefetch_map: HashMap::new(),
        }
    }
    fn canary_name(&self) -> &str {
        self.program_info.canary_export_name()
    }
    fn write_main_import(&mut self, mod_path: &str) -> Result<()> {
        match self.input_options.target {
            crate::OutputTarget::Web(_) => writeln!(
                &mut self.javascript,
                r#"import {{ initSync }} from "{}";
"#,
                mod_path
            )?,
            crate::OutputTarget::Bundler(_) => writeln!(
                &mut self.javascript,
                r#"import * as __wasm from "{}";
"#,
                mod_path
            )?,
        }
        Ok(())
    }
    fn write_get_shared_imports(&mut self, main_shares: &str) -> Result<()> {
        let canary_props = if self.input_module.options.debug_assertions {
            format!("{}: ~0xdead,", self.canary_name())
        } else {
            String::new()
        };
        let main_exports = match self.input_options.target {
            crate::OutputTarget::Web(_) => "initSync(undefined, undefined)",
            crate::OutputTarget::Bundler(_) => "__wasm",
        };
        Ok(write!(
            &mut self.javascript,
            r#"let sharedImports = undefined;
function getSharedImports() {{
    if (sharedImports === undefined) {{
        sharedImports = {{ __wasm_split: {{ {canary_props} }} }};
        const {{ {main_shares} }} = {main_exports};
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
        if self.input_module.options.debug_assertions {
            self.javascript
                .push_str(include_str!("./snippets/instantiate.debug.js"));
        }
        Ok(())
    }
    fn write_export_const(&mut self, name: &str, def: &impl std::fmt::Display) -> Result<()> {
        Ok(writeln!(
            &mut self.javascript,
            "export const {name} = {def};"
        )?)
    }
    fn fetcher<'pth>(&self, empty: bool, file_path: impl 'pth + std::fmt::Display) -> String {
        if empty {
            return "() => async (_imp) => ({})".to_string();
        }
        let wrap = if self.input_module.options.debug_assertions {
            "debugWrap"
        } else {
            ""
        };
        // Note: the expression returned from here should:
        // - allow lazily fetching the wasm module (no top-level import)
        // - allow bundlers and downstream code to recognize it as an expression to a path ("relocate" the import)
        match self.input_options.target {
            // Note: we use the form `new URL(<string literal>, import.meta.url)` which is understood by some
            // bundlers as syntax that can get rewritten if the path from where the file gets fetched is changed
            // (for example due to attaching a has of its contents).
            crate::OutputTarget::Web(_) => format!(
                r#"() => {{
    const src = fetch(new URL({file_path}, import.meta.url));
    return async (imports) => {wrap}(WebAssembly.instantiateStreaming(src, imports));
}}
"#
            ),
            crate::OutputTarget::Bundler(_) => format!(
                r#"() => {{
    const module = import.source({file_path});
    return async (imports) => {wrap}(new WebAssembly.Instance(await module, imports));
}}
"#
            ),
        }
    }
    fn write_loaders(&mut self, program: &SplitProgramInfo) -> Result<()> {
        let mut split_deps = HashMap::<String, Vec<String>>::new();
        for (module_index, (name, output_module)) in program.output_modules.iter().enumerate() {
            let SplitModuleIdentifier::Chunk(splits) = name else {
                continue;
            };
            let is_empty = output_module.is_empty;
            let file_name = name.filename(module_index);
            let var_name = format!("__chunk_{module_index}");
            let splits_dbg = splits.iter().cloned().collect::<Vec<_>>().join(", ");
            writeln!(&mut self.javascript, "/* {splits_dbg} */")?;
            let fetcher = self.fetcher(is_empty, format_args!("\"./{file_name}.wasm\""));
            writeln!(
                &mut self.javascript,
                "const {var_name} = makeLoad({fetcher}, []);"
            )?;
            for split in splits {
                split_deps
                    .entry(split.clone())
                    .or_default()
                    .push(var_name.clone());
                if !is_empty {
                    self.prefetch_map
                        .entry(split.clone())
                        .or_default()
                        .push(file_name.clone());
                }
            }
        }
        for (module_index, (identifier, output_module)) in
            program.output_modules.iter().enumerate().rev()
        {
            let split = match &identifier {
                SplitModuleIdentifier::Main | SplitModuleIdentifier::Chunk(_) => continue,
                SplitModuleIdentifier::Split(split) => split,
            };
            let is_empty = output_module.is_empty;
            let file_name = identifier.filename(module_index);
            let loader_name = identifier.loader_name();
            let deps = split_deps.remove(split).unwrap_or_default();
            let deps = deps.join(", ");
            let fetch_opts = self.fetcher(is_empty, format_args!("\"./{file_name}.wasm\""));
            self.write_export_const(
                &loader_name,
                &format_args!("wrapAsyncCb(makeLoad({fetch_opts}, [{deps}]))"),
            )?;
            let prefetches = self.prefetch_map.entry(split.clone()).or_default();
            if !is_empty {
                prefetches.push(file_name);
            }
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

pub fn link_module<'p>(
    main_module_path: &str,
    program_info: &'p SplitProgramInfo,
    emit_state: &'p EmitState,
) -> Result<LinkModuleWriter<'p>> {
    let js_emit_span = perf_span!("emit js");
    let _emit_span = js_emit_span.enter();
    let mut link_module = LinkModuleWriter::new(program_info, emit_state);

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
