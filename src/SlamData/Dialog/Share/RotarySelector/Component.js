// module SlamData.Dialog.Share.RotarySelector.Component
exports.getComputedStyle = function(el) {
    return function() {
        return getComputedStyle(el);
    };
};

exports.toString = function(base) {
    return function(num) {
        return num.toString(base);
    };
};
