// module Control.UI.File

exports.fileListToArray = function(fl) {
    var result = [];
    for (var i = 0; i < fl.length; i++) {
        result.push(fl[i]);
    }
    return result;
};

exports.name = function(file) {
    return function() {
        return file.name;
    };
};

exports.newReaderEff = function() {
    return new FileReader();
};

exports.readAsBinaryStringEff = function(file) {
    return function(reader) {
        return function() {
            reader.readAsBinaryString(file);
        };
    };
};

exports.resultImpl = function(nothing, just, fr) {
    return function() {
        var res = fr.result;
        if (res == null) return nothing;
        return just(res);
    };
};

exports.filesEff = function(el) {
    return function() {
        if (!el.files) return [];
        return el.files;
    };
};

exports.onloadEff = function(reader) {
    return function(action) {
        return function() {
            reader.onload = function(ev) {
                action();
            };
        };
    };
};
