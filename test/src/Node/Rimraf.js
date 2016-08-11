var rm = require('rimraf');

exports.rimraf = function(str) {
    return function(cb, eb) {
        try {
            rm(str, [], cb);
        } catch (e) {
            eb(e);
        }
    };
};
