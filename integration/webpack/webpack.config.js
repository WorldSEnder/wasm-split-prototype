const path = require("path");
const CopyPlugin = require("copy-webpack-plugin");
const { WasmPackPlugin } = require("./plugin/index.js");

const dist = path.resolve(__dirname, "dist");

module.exports = {
  entry: {
    index: "./js/index.js"
  },
  output: {
    path: dist,
    filename: "[name].js",
    clean: true,
  },
  devServer: {
    static: {
      directory: dist,
    },
    headers: {
      "Access-Control-Allow-Origin": "*",
    },
  },
  plugins: [
    new CopyPlugin({
      patterns: [
        { from: path.resolve(__dirname, "static"), to: dist },
      ]
    }),

    new WasmPackPlugin({
      crateDirectory: path.resolve(__dirname, "../simple"),
    }),
  ],
  experiments: {
    asyncWebAssembly: true,
    sourceImport: true,
  }
};
