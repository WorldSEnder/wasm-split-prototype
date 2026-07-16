// Using wasm-pack is currently not an option as it doesn't support wasm-split
// Hence, we dog-food a bit and write our own plugin utility for webpack

import { spawn } from 'node:child_process';
import * as fs from 'node:fs';
import * as path from 'node:path';
import {fileURLToPath} from 'node:url';

import chalk from 'chalk';
import Watchpack from 'watchpack';

import { Transform } from 'node:stream';
import { pipeline } from 'node:stream/promises';
import { text } from 'node:stream/consumers';
import { streamValues } from 'stream-json/streamers/stream-values.js';

const error = (msg) => console.error(chalk.bold.red(msg))
let info = (msg) => console.log(chalk.bold.blueBright(msg))
function dbg(obj) { info(JSON.stringify(obj)); return obj }

class Compilation {
    constructor(plugin) {
        this._plugin = plugin;
        this._split_proto_dir = path.resolve(fileURLToPath(import.meta.url), '../../../..');
    }
    async _build() {
        await this._step_run_cargo()
        await this._step_split()
        await this._step_wasm_bindgen()
    }
    async _step_run_cargo() {
        const profile = this._plugin.mode == "development" ? "dev" : "release";
        const cargo = new Process('cargo',
            ['build',
                '--profile', profile,            
                '--target', 'wasm32-unknown-unknown',
                '--message-format=json-render-diagnostics',
            ],
        { cwd: this._plugin.crateDirectory });
        let artifact;
        wrapJsonLStream(cargo.stdout).on('data', ({value:m}) => {
            const msg = JSON.stringify(m);
            if (m.reason == "compiler-artifact" && m.target.kind.includes('cdylib')) {
                artifact = m.filenames[0];
            }
        });
        await cargo.done()
        if (!artifact) {
            throw new Error('Expected a compiler artifact, but got none after successful compilation');
        }
        this._wasm_path = artifact;
    }
    async _step_split() {
        const cargo = new Process('cargo', 
            ['+nightly', 'run', '-p', 'wasm_split_cli_support', '--all-features',
                '--',
                '--target', 'bundler',
                '--out-name', this._plugin.outName,
                this._wasm_path,
                this._plugin.outDir,
            ],
            { cwd: this._split_proto_dir,
              env: { WASM_SPLIT_CLI_ENABLE_DWARF: "1", ...process.env },
            }
        );
        await cargo.done()
        info(`Split ${this._wasm_path} successfully`);
        this._wasm_path = path.resolve(this._plugin.outDir, `${this._plugin.outName}.wasm`);
    }
    async _step_wasm_bindgen() {
        const cargo = new Process('cargo',
            ['run', '--bin', 'wasm-bindgen-cli-wrapper',
                '--',
                '--target', 'bundler',
                '--out-name', this._plugin.outName,
                '--keep-lld-exports', '--keep-debug',
                '--out-dir', this._plugin.outDir,
                this._wasm_path,
            ],
            { cwd: path.resolve(this._split_proto_dir, 'test-runner'),
            },
        );
        await cargo.done()
    }
}

export class WasmPackPlugin {
    constructor(options) {
        /**
         * In some cases Webpack will require the pkg entrypoint before it actually
         * exists. To mitigate that we are forcing a first compilation.
         *
         * See https://github.com/wasm-tool/wasm-pack-plugin/issues/15
         */
        this._initialCompilation = undefined;
        const { crateDirectory, outDir = 'pkg', outName = 'index', forceMode, forceWatch } = options;
        this.crateDirectory = crateDirectory;
        this.wp = new Watchpack();
        this.mode = forceMode;
        this.forceWatch = forceWatch;
        this.outDir = path.resolve(outDir);
        this.outName = outName;
        // TODO: resolve from Cargo.toml
        this.watchDirectories = (options.watchDirectories || []).concat(
            path.resolve(this.crateDirectory, 'src')
        )
        this.watchFiles = [
            path.resolve(this.crateDirectory, 'Cargo.toml'),
            fileURLToPath(import.meta.url), // Watch this plugin
        ];
        this.activeCompilation = Promise.resolve();
    }
    _checkWasmPack() {}
    async _compile(watching) {
        const compilation = new Compilation(this);
        await compilation._build();
    }
    async _initialCompile(compiler) {
        await this._checkWasmPack();
        const shouldWatch =
            Boolean(this.forceWatch ?? compiler.watchMode);
        if (shouldWatch) {
            this.wp.watch({
                files: this.watchFiles,
                directories: this.watchDirectories,
                startTime: Date.now() - 10000,
            })

            this.wp.on('aggregated', () => {
                this.activeCompilation = this._compile(true);
                this.activeCompilation.catch(() => {});
            })
        }

        this.activeCompilation = this._compile(false);
        // ignore any errors here, they are aggregated later
        return this.activeCompilation.catch(() => {});
    }
    apply(compiler) {
        this.mode = this.mode ?? compiler.options.mode;
        // This fixes an error in Webpack where it cannot find
        // the `pkg/index.js` file if Rust compilation errors.
        this._makeEmpty()
        // force first compilation
        compiler.hooks.beforeCompile.tapPromise('WasmPackSplitPlugin', () => {
            this._initialCompilation = this._initialCompilation || this._initialCompile(compiler);
            return this._initialCompilation;
        })

        compiler.hooks.make.tapPromise('WasmPackSplitPlugin', async (compilation) => {
            // This is needed in order to gracefully handle errors in Webpack,
            // since Webpack has its own custom error system.
            let error;
            try {
                await this.activeCompilation;
            } catch (e) { error = e }
            if (error != null) {
                compilation.errors.push(error)
            }
        })
    }
    _makeEmpty() {
        fs.mkdirSync(this.outDir, { recursive: true });
        fs.writeFileSync(path.join(this.outDir, this.outName + '.js'), `throw new Error('should be overwritten')`);
    }
}


class Process {
    constructor(bin, args, options) {
        this._p = new Promise((resolve, reject) => {
            const p = this._process = spawn(bin, args, options)

            p.on('close', (code) => {
                if (code === 0) {
                    resolve()
                } else {
                    reject(new Error('Rust compilation failed.'))
                }
            })

            p.on('error', reject)
        })
        this._errorLog = text(this.stderr);
    }
    get stdout() { return this._process.stdout }
    get stderr() { return this._process.stderr }
    async done() {
        try {
            await this._p;
        } catch (e) {
            error(await this._errorLog);
            throw e
        }
    }
}
async function runProcess(bin, args, options) {
    return new Process(bin, args, options).done()
}
function wrapJsonLStream(dataIn) {
    return dataIn
        .pipe(streamValues.withParserAsStream({ jsonStreaming: true }));
}
