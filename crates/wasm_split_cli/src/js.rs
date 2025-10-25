use eyre::Result;
use std::{collections::HashMap, fmt::Write, path::Path};

use crate::split_point::{SplitModuleIdentifier, SplitProgramInfo};

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
            write!(
                &mut self.javascript,
                "const {var_name} = makeLoad(\"./{file_name}.wasm\", []);\n"
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
            self.write_export_const(
                &loader_name,
                &format_args!("wrapAsyncCb(makeLoad(\"./{file_name}.wasm\", [{deps}]))"),
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

pub fn link_module(
    main_module: &str,
    split_program_info: &SplitProgramInfo,
) -> Result<LinkModuleWriter> {
    let mut link_module = LinkModuleWriter::new();
    link_module.write_main_import(main_module)?;
    link_module.write_runtime()?;
    link_module.write_loaders(split_program_info)?;
    Ok(link_module)
}
