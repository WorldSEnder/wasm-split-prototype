function makeFetch(url) {
    const srcUrl = new URL(url, import.meta.url);
    return () => {
        const src = fetch(srcUrl);
        return async (imports) => {
            return await WebAssembly.instantiateStreaming(src, imports);
        }
    }
}
