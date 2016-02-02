// module SlamData.Config.Version

// This path is relative to the tmp directory during bundling, not the actual
// location of this current file
var packageInfo = require("../package.json");

exports.slamDataVersion = packageInfo.version;
