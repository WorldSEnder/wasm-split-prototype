
let sharedImports = undefined;
function getSharedImports() {
    if (sharedImports === undefined) {
        const mainExports = initSync(undefined, undefined);
        sharedImports = {
            __wasm_split: {
                __indirect_function_table: mainExports.__indirect_function_table,
                __stack_pointer: mainExports.__stack_pointer,
                memory: mainExports.memory,
            },
        };
    }
    return sharedImports;
}
function wrapAsyncCb(callee) {
    return async (callbackIndex, callbackData) => {
        let success;
        try {
            await callee();
            success = true;
        } catch (e) {
            console.error(e);
            success = false;
        } finally {
            const sharedImports = getSharedImports();
            sharedImports.__wasm_split.__indirect_function_table.get(callbackIndex)(callbackData, success);
        }
    }
}
function makeLoad(url, deps) {
    const loader = async () => {
        const parallelStuff = deps.map(d => d());
        const fetchSelf = fetch(url);
        parallelStuff.push(fetchSelf);
        await Promise.all(parallelStuff);
        const response = await fetchSelf;
        const imports = getSharedImports();
        return await WebAssembly.instantiateStreaming(response, imports);
    };
    let loadingModule = undefined;
    return () => {
        if (loadingModule === undefined) loadingModule = loader();
        return loadingModule;
    }
}