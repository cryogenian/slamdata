// module Utils.SessionStorage

exports.setSessionStorageImpl = function(key, value) {
    return function() {
        window.sessionStorage.setItem(key, value);
    };
};

exports.getSessionStorageImpl = function(Nothing, Just, key) {
    return function() {
        var result = window.sessionStorage[key];
        if (typeof result === "undefined") {
            return Nothing;
        }
        else {
            return Just(result);
        }
    };
};

