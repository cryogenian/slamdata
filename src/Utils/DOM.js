// module Utils.DOM

exports.waitLoaded = function(cb, eb) {
    if (window.document.readyState === "complete") {
        return cb({});
    } else {
        return window.addEventListener("load", cb);
    }
};

exports.onLoad = function(action) {
    return function() {
        window.addEventListener("load", function() {
            action();
        });
    };
};

exports.blur = function(el) {
    return function() {
        return el.blur();
    };
};

exports.focus = function(el) {
    return function() {
        return el.focus();
    };
};

exports.getBoundingClientRect = function(el) {
    return function() {
        return el.getBoundingClientRect();
    };
};

exports.offsetLeft = function(el) {
    return function() {
       return el.offsetLeft;
    };
};
