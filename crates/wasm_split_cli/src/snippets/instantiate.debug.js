async function debugWrap(instantiate) {
    try {
        return await instantiate;
    } catch (e) {
        if (e instanceof WebAssembly.LinkError && e.message.includes("__canary")) {
            console.error("wasm-split: It looks like you are trying to load a split module that does not belong to the same compilation unit. This is not supported.")
        }
        throw e;
    }
}
