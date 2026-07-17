function assert(value, msg = undefined) {
    if (!value) {
        let message = msg ?? "assertion failed";
        throw new Error(message);
    }
}

describe('Simple', () => {
    it('should load', async () => {
        await import('./index');
    })
    it('should compute', async () => {
        const { call_args_test } = await import('./index');
        const result = await call_args_test(10, 20);
        assert(result == 30, 'the sum of 10 + 20 should be 30');
    })
})
