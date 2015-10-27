// module Control.UI.Browser.Event

exports.raiseEventImpl = function(name, node) {
    return function() {
        if (name === "click") {
            return node.click();
        }
        var evt = new CustomEvent(name);
        evt.initEvent(name, false, true);
        node.dispatchEvent(evt);
        return node;
    };
};
