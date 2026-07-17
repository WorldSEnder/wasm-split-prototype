const config = require('./webpack.config');

module.exports = {
    ...config,
    mode: 'development',
    target: 'node',
    output: {
        ...config.output,
        publicPath: "/",
    }
};