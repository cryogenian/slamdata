// module Utils.DOM

exports.waitLoaded = function(cb, eb) {
    window.addEventListener("load", cb);
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

exports.getComputedStyle = function(el) {
    return function() {
        return getComputedStyle(el);
    };
};

exports.getClientRects = function(el) {
    return function() {
        var rects = el.getClientRects(),
            result = [];
        for (var i = 0; i < rects.length; i++) {
            result.push(rects[i]);
        }
        return result;
    };
};
