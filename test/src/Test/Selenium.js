// module Test.Selenium

exports.makePublic = function (name) {
  return function (fn) {
    return function () {
      module.exports[name] = fn;
    };
  };
};
