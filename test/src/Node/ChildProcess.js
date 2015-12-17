// module Node.ChildProcess

var cp = require("child_process");

exports.stdoutImpl = function(p) {
    return function() {
        return p.stdout;
    };
};

exports.stdinImpl = function(p) {
    return function() {
        return p.stdin;
    };
};

exports.stderrImpl = function(p) {
    return function() {
        return p.stderr;
    };
};

exports.spawnImpl = function(name, args, opts, effHandler, Just, Nothing) {
    return function() {
        var result = cp.spawn(name, args, opts, function(err) {
            if (err) {
                return effHandler(Just(err))();
            }
            else {
                return effHandler(Nothing)();
            }
        });
        return result;
    };
};

exports.emptySpawnOptions = {};

exports.execSyncImpl = function(name, opts) {
    return function() {
        cp.execSync(name, opts);
    };
};


exports.killImpl = function(pr) {
    return function() {
        pr.kill();
    };
};
