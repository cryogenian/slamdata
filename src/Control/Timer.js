// module Control.Timer

exports.timeout = function(time) {
    return function(action) {
        return function() {
            return setTimeout(function() {
                action();
            }, time);
        };
    };
};

exports.clearTimeout = function(to) {
    return function() {
        return clearTimeout(to);
    };
};

exports.interval = function(time) {
    return function(action) {
        return function() {
            return setInterval(function() {
                action();
            }, time);
        };
    };
};

exports.clearInterval = function(i) {
    return function() {
        return clearInterval(i);
    };
};
