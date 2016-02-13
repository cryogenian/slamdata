// module Utils.LocalStorage

exports.setLocalStorageImpl = function(key, value) {
    return function() {
        window.localStorage.setItem(key, value);
    };
};

exports.getLocalStorageImpl = function(Nothing, Just, key) {
    return function() {
        var result = window.localStorage[key];
        if (typeof result === "undefined") {
            return Nothing;
        }
        else {
            return Just(result);
        }
    };
};
