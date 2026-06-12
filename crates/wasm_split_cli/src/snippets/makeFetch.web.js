function makeFetch(srcUrl) {
    if (srcUrl === undefined) {
        return () => { return async (_imports) => { return {} } };
    }
    return () => {
        // Start the first fetch eagerly so the download overlaps with loading
        // this module's dependencies.
        let src = fetch(srcUrl);
        return async (imports) => {
            // A transient network failure (flaky connection, aborted request,
            // momentary 5xx) shouldn't fail the whole load: retry a couple of
            // times with a short backoff before reporting failure.
            const RETRIES = 2;
            const BASE_DELAY_MS = 250;
            for (let attempt = 0; ; attempt++) {
                try {
                    return await WebAssembly.instantiateStreaming(src, imports);
                } catch (e) {
                    if (attempt >= RETRIES) throw e;
                    await new Promise(resolve => setTimeout(resolve, BASE_DELAY_MS * 2 ** attempt));
                    // A Response body is single-use; retries need a new fetch.
                    src = fetch(srcUrl);
                }
            }
        }
    }
}
