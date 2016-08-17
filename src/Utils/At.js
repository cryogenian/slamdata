// module Utils.At

exports.setInterval = function (windowReferenceObject) {
  return function (ms) {
    return function (f) {
      return function() {
        var interval = windowReferenceObject.setInterval(f(interval), ms);
        return interval;
      };
    };
  };
};

exports.clearInterval = function (windowReferenceObject) {
  return function(interval) {
    return function() {
      return windowReferenceObject.clearInterval(interval);
    };
  };
};
