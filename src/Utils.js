// module Utils

exports.prettyJson = function (json) {
  return JSON.stringify(json, null, 2);
};

exports.isFirefox = function () {
  typeof InstallTrigger !== 'undefined';
};
