var path = require("path");
var baseConf = {
  devtool: null,
  module: {
    loaders: [
      { test: /\.less$/, loader: "less-loader" },
      { test: /\.json$/, loader: "json-loader" }
    ]
  },
  resolve: {
    alias: {
      "package.json": path.join(__dirname, "package.json")
    }
  }
};
module.exports = [
  Object.assign(baseConf, {
    entry: {
      workspace: "./webpack.entry.workspace.js",
      filesystem: "./webpack.entry.filesystem.js",
      auth_redirect: "./webpack.entry.auth_redirect.js",
    },
    output: {
      filename: "./public/js/[name].js"
    }
  })
];
