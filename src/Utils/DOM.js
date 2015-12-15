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
