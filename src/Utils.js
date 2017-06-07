// module Utils

exports.prettyJson = function (json) {
  return JSON.stringify(json, null, 2);
};

exports.isFirefox = function () {
  typeof InstallTrigger !== 'undefined';
};

exports.debugTime = function() {
    return function(msg) {
        return function(thunk) {
            var start = (new Date()).getTime();
            var result = thunk();
            var end = (new Date()).getTime();
            console.log(msg + ": " + (end - start));
            return result;
        };
    };
};
