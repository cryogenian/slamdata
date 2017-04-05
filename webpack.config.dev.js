var path = require('path')
var webpack = require('webpack');

var dirImports = path.join(__dirname, 'script');

module.exports = {
  devtool: false,
  entry: {
    workspace: path.join(dirImports, 'importWorkspace.js'),
    auth_redirect: path.join(dirImports, 'importAuthRedirect.js'),
    filesystem: path.join(dirImports, 'importFileSystem.js'),
  },
  output: {
     filename: './public/js/[name].js'
  },
  resolve: {
    alias: {
      "package.json": path.join(__dirname, "package.json")
    },
  }
};
