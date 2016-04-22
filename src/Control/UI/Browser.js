// module Control.UI.Browser

exports.locationString = function() {
    var path = window.location.pathname.split("/");
    path.pop();
    return window.location.origin + path.join("/");
};

exports.select = function(el) {
    return function() {
        el.select();
    };
};
exports.newTab = function(url) {
    return function() {
        window.open(url, "_blank");
    };
};

exports.clearValue = function(el) {
    return function() {
        el.value = null;
    };
};

exports.setTitle = function(t) {
    return function() {
        document.title = t;
    };
};
