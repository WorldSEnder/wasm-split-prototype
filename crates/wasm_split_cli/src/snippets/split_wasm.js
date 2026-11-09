function wrapAsyncCb(callee) {
    return (callbackIndex, callbackData) => {
        let success = false;
        callee().then(() => { success = true }, e => console.error(e))
            .finally(() => getSharedImports().__wasm_split.__indirect_function_table.get(callbackIndex)(callbackData, success))
    }
}
function makeLoad(fetcher, deps) {
    const loader = async () => {
        const parallelStuff = deps.map(d => d())
        const src = fetcher()
        src.catch(() => 0)
        await Promise.all(parallelStuff)
        return instantiate(src)
    };
    let loadingModule;
    return () => {
        if (loadingModule === undefined) {
            const thisLoad = loadingModule = loader()
            thisLoad.catch(() => {
                if (loadingModule === thisLoad) loadingModule = undefined;
            })
        }
        return loadingModule
    }
}
