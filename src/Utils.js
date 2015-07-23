// module Utils

exports.newTab = function(url) {
    return function() {
        window.open(url, "_blank");
    };
};

exports.mailOpen = function(url) {
    return function() {
        window.open(url);
    };
};

exports.reload = function() {
    document.location.reload();
};


exports.clearValue = function(el) {
    return function() {
        el.value = null;
    };
};

exports.select = function(el) {
    return function() {
        el.select();
    };
};

exports.locationParent = function(loc) {
    return function() {
        var path = loc.pathname.split("/");
        path.pop();
        return loc.origin + path.join("/");
    };
};

exports.replaceLocation = function(url) {
    return function() {
        window.location.replace(url);
    };
};

exports.encodeURIComponent = window.encodeURIComponent;
exports.decodeURIComponent = window.decodeURIComponent;
