// module Utils.Trace

exports._addTime = function(Tuple) {
    return function(thunk) {
        var start, end, result;
        start = (new Date()).getTime();
        result = thunk();
        end = (new Date()).getTime();
        return Tuple(end - start)(result);
    };
};
