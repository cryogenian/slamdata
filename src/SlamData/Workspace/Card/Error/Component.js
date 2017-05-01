"use strict";

exports.prettify = function (x) {
  return JSON.stringify(x, null, 2);
};
