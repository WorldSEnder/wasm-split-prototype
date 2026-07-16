const config = require('./webpack.config');

module.exports = {
    ...config,
    target: 'node',
    output: {
        ...config.output,
        publicPath: "/",
    }
};