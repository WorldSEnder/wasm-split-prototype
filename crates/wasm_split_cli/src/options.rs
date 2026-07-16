use std::path::Path;

/// Web target specific options
#[derive(Default)]
pub struct WebTargetOptions {
    _priv: (),
}

/// Bundler target specific options
#[derive(Default)]
pub struct BundlerTargetOptions {
    pub(crate) _source_imports: bool,
}

/// Target specific options
#[non_exhaustive]
pub enum OutputTarget {
    Web(WebTargetOptions),
    Bundler(BundlerTargetOptions),
}

impl Default for OutputTarget {
    fn default() -> Self {
        Self::Web(Default::default())
    }
}

impl OutputTarget {
    /// Change the target to "web".
    ///
    /// This target supports javascript modules, but instantiates
    /// wasm modules with manually configured imports.
    pub fn web(&mut self) -> &mut WebTargetOptions {
        loop {
            if let Self::Web(web) = self {
                break web;
            }
            *self = Self::Web(Default::default());
        }
    }
    /// Change the target to "bundler".
    ///
    /// This target is geared towards webpack. Wasm modules derive
    /// their imports from the declared import paths.
    pub fn bundler(&mut self) -> &mut BundlerTargetOptions {
        loop {
            if let Self::Bundler(bundler) = self {
                break bundler;
            }
            *self = Self::Bundler(Default::default())
        }
    }
}

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
    /// Output target
    ///
    /// Default: Web
    pub target: OutputTarget,
    /// The meaning of this depends on the target:
    /// - for web, this is that module path from where `initSync` will be imported from
    /// - for bundler this is the module path to the wasm module
    ///
    /// Default: `"./main.js"`
    pub main_module: &'a str,
    /// Verbosely output additional information about processing.
    ///
    /// Default: false
    pub verbose: bool,
    /// Switch to transform and emit `.debug_` sections.
    ///
    /// This option is experimental.
    /// Default: `true` if the `WASM_SPLIT_CLI_ENABLE_DWARF` environment variable is non-empty.
    pub emit_dwarf: bool,
    /// Enables explicit tests for assumptions we make about the input wasm file during integration testing.
    #[doc(hidden)]
    pub strict_tests: bool,
}

impl<'wasm> Options<'wasm> {
    /// New default options of the specified input wasm.
    pub fn new(input_wasm: &'wasm [u8]) -> Self {
        Self {
            input_wasm,
            output_dir: Path::new("wasm_split"),
            main_out_path: Path::new("wasm_split/main.wasm"),
            link_name: "./__wasm_split.js",
            main_module: "./main.js",
            target: OutputTarget::default(),
            verbose: false,
            emit_dwarf: std::env::var_os("WASM_SPLIT_CLI_ENABLE_DWARF")
                .is_some_and(|v| !v.is_empty()),
            strict_tests: false,
        }
    }
}
