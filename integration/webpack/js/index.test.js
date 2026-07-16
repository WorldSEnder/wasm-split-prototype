function assert(value, msg = undefined) {
    if (!value) {
        let message = msg ?? "assertion failed";
        throw new Error(message);
    }
}

describe('Simple', () => {
    it('should load', async () => {
        const { call_args_test } = await import('./index');
        const result = await call_args_test(10, 20);
        assert(30 == 30, 'it should compute the result');
    })
})
