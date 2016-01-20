// module Utils.LocalStorage

exports.setLocalStorageImpl = function(key, value) {
    return function() {
        localStorage.setItem(key, value);
    };
};

exports.getLocalStorageImpl = function(Nothing, Just, key) {
    return function() {
        var result = localStorage[key];
        if (typeof result === "undefined") {
            return Nothing;
        }
        else {
            return Just(result);
        }
    };
};
