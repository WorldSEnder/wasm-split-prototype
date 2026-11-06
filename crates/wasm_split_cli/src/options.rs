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

/// Merge chunks under a simple probablistic loading cost model.
///
/// Each split is assumed to be loaded independently from each other, and independent
/// of the code size of the split, according to a fixed probability.
#[derive(Clone, Copy, Debug)]
pub struct BernoulliCost {
    /// The equivalent byte count that can be fetched and loaded in the time it takes
    /// to make a network request.
    ///
    /// Approximate values could be 10ms waiting on the connection, 2MiB/s fetch and load,
    /// leading to an equivalent request cost of ~20.5 kiB.
    ///
    /// The current default uses a round 16 kiB equivalent cost.
    pub request_cost: usize,
    /// The chance that one split module gets loaded.
    pub load_probability: f64,
}

/// Used in the optimizer for chunk merging to avoid a large number of chunks.
#[derive(Debug)]
#[non_exhaustive]
pub enum CostModel {
    /// Keep every chunk as-is, disables chunk merging. This is equivalent to assuming
    /// that a chunk load has no cost.
    DisableMerging,
    /// Merge chunks when that lowers the expected loading cost. Each split module's
    /// loading state is modelled as a [binary random variable].
    ///
    /// [binary random variable]: https://en.wikipedia.org/wiki/Bernoulli_distribution
    Binary(BernoulliCost),
}

impl OutputTarget {
    fn default_cost_model(&self) -> CostModel {
        // Chunks of the web target are fetched over the network, so we use the bernoulli loading model here.
        // A bundler decides on its own how chunks get loaded.
        match self {
            Self::Web(_) => CostModel::Binary(BernoulliCost {
                request_cost: 16 * 1024,
                load_probability: 0.05,
            }),
            Self::Bundler(_) => CostModel::DisableMerging,
        }
    }
}

impl CostModel {
    pub(crate) fn validate(&self) -> eyre::Result<()> {
        match self {
            Self::DisableMerging => {}
            Self::Binary(model) => {
                eyre::ensure!(
                    0. <= model.load_probability && model.load_probability <= 1.,
                    "expected a load probability between 0 and 1, got {}",
                    model.load_probability
                );
            }
        }
        Ok(())
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
    /// A cost model under which chunk optimization is performed.
    ///
    /// Default: `None`, which picks a default model based on the output target.
    /// This default model is not subject to semver stability guarantees.
    pub cost_model: Option<CostModel>,
    /// Enables explicit tests for assumptions we make about the input wasm file during integration testing.
    #[doc(hidden)]
    pub strict_tests: bool,
}

impl<'wasm> Options<'wasm> {
    pub(crate) fn default_cost_model(&self) -> CostModel {
        self.target.default_cost_model()
    }

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
            cost_model: None,
            strict_tests: false,
        }
    }
}
