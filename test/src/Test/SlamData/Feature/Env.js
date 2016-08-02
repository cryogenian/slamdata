exports.getEnv = function (key) {
  return function () {
    var val = process.env[key];
    if (typeof val !== "undefined") {
      return val;
    }

    throw new Error("Environment variable '" + key + "' not set");
  };
};
