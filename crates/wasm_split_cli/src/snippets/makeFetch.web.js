function makeFetch(srcUrl) {
    return () => {
        const src = fetch(srcUrl);
        return async (imports) => {
            return await WebAssembly.instantiateStreaming(src, imports);
        }
    }
}
