// Probably should be putted to gulpfile.js
var childProcess = require('child_process'),
    mongo = require('mongodb').MongoClient,
    config = require("./config.json"),
    fs = require('fs'),
    path = require('path'),
    minimist = require('minimist'),
    lodash = require('lodash'),
    chalk = require('chalk');


var _ = lodash;
var args = minimist(process.argv.slice(2));
var log = function(str) {
    if (!args.silent && !args.s) {
        if (str.stack) {
            console.log(str.stack);
        }
        console.log(chalk.gray(str));   
    }
};
config.upload.filePath = path.resolve(config.upload.file);

// decodeURIComponent in Match.Route
window = global;


if (args.b) {
    config.selenium.browser = args.b;
}

var url = config.connectionString + config.database.name,
    restoreCmd = config.restoreCmd,
    slamengineArgs = ['-jar',
                      path.resolve(config.slamengine.jar),
                      '-c',
                      path.resolve(config.slamengine.config)
                     ],
    seleniumArgs = ['-jar',
                    path.resolve(config.selenium.jar)];


function links(cb) {
    var testNM = path.resolve("./test/node_modules"),
        outputNM = path.resolve("./output/node_modules"),
        output = path.resolve("./output");
    fs.unlink(testNM, function(err) {
        fs.unlink(outputNM, function(err) {
            fs.symlink(output, outputNM, function(err) {
                fs.symlink(output, testNM, function(err) {
                    cb();
                });
            });
        });
    });
}

function main() {
    var slamEngine, selenium, launched = {}, running = false;
    var clean = function(code) {
        selenium.kill();
        log("selenium killed");
        slamEngine.kill();
        log("slamengine killed");
        process.exit(code);
    };

    var run = function() {
        if (!launched.engine || !launched.selenium) {
            return;
        }
        setTimeout(function() {
            if (running) return;
            running = true;
            mongo.connect(url, function(err, db) {
                log("running tests\n\n");
                try {
                    require(config.selenium.main).test(config)(function() {
                        log("\n\ncleaning");
                        db.dropDatabase();
                        db.close();
                        log("database dropped");
                        clean(0);
                    }, function(err) {
                        log(err, err.stack.split("\n"));
                        clean(1);
                    });
                } catch(err) {
                    log(err, err.stack.split("\n"));
                }

            });
        }, 1000);
    };
    try {
        log("symlinking");
        links(function() {
            log("symlinks created");
            mongo.connect(url, function(err, db) {
                db.dropDatabase(function() {
                    log("database dropped");
                    selenium = childProcess.spawn('java', seleniumArgs, {}, function(err) {
                        if (err) {
                            log(err, err.stack.split('\n'));
                            clean(1);
                        }
                    });
                    slamEngine = childProcess.spawn('java', slamengineArgs, {}, function(err) {
                        if (err) {
                            log(err, err.stack.split('\n'));
                            clean(1);
                        }
                    });
                    log("restoring test database");
                    childProcess.execSync(restoreCmd);
                    log("database restored");
                    selenium.stderr.on("data", function(data) {
                        if (/Selenium Server is up and running/.test(data + '')) {
                            log("selenium launched");
                            launched.selenium = true;
                            run();
                        }
                    });
                    slamEngine.stdout.on('data', function(data) {
                        if (/Embedded server listening at port 8080/.test(data + '')) {
                            log("slamengine launched");
                            launched.engine = true;
                            run();
                        }
                    });
                });
            });
        });
    }
    catch (e) {
        log("An error occured");
        log(e.message, e.stack.split("\n"));
        clean(1);
    }
    process.on('exit', function() {clean(0);});
}

main();

