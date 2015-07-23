// module Utils.Event

exports.raiseEventImpl = function(name, node) {
    return function() {
        var evt = new CustomEvent(name);
        evt.initEvent(name, true, true);
        node.dispatchEvent(evt);
        return node;
    };
};
