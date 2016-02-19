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

exports.encodeURIComponent = function(str) {
    if (typeof window !== "undefined") {
        return window.encodeURIComponent(str);
    } else {
        return global.encodeURIComponent(str);
    }
};

exports.decodeURIComponent = function(str) {
    if (typeof window !== "undefined") {
        return window.decodeURIComponent(str);
    } else {
        return global.decodeURIComponent(str);
    }
};

exports.getScreen = function() {
    var ws = window.screen;
    return {
        width: ws.width,
        height: ws.height
    };
};
