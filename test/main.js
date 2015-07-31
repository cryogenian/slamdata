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
                      config.slamengine.jar,
                      '-c',
                      config.slamengine.config
                     ],
    seleniumArgs = ['-jar',
                    config.selenium.jar];


function links(cb) {
    fs.unlink(path.resolve("./test/node_modules"), function(err) {
        fs.unlink(path.resolve("./output/node_modules"), function(err) {
            fs.symlink(path.resolve("./output"), path.resolve("./output/node_modules"), function(err) {
                fs.symlink(path.resolve("./output"), path.resolve("./test/node_modules"), function(err) {
                    cb();
                });
            });
        });
    });
}

function main() {
    var slamEngine, selenium, i, launched = {}, running = false;
    var clean = function(code) {
        clearInterval(i);
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
            mongo.connect(url, function(err, db) {
                db.dropDatabase(function() {
                    selenium = childProcess.spawn('java', seleniumArgs, {}, function(err) {
                        log(err, err.stack.split('\n'));
                        clean(1);
                    });
                    slamEngine = childProcess.spawn('java', slamengineArgs, {}, function(err) {                
                        log(err, err.stack.split('\n'));
                        clean(1);
                    });
                    
                    log("restoring test database");
                    childProcess.execSync(restoreCmd);
                    log("database restored");
                    i = setInterval(function() {
                        if (launched.engine && launched.selenium) {
                            clearInterval(i);
                            return;
                        }
                        
                        var ps = childProcess.spawn('ps', ['aux'], {}, function(err) {
                            log("Can't get ps");
                        });
                        ps.stdout.on('data', function(data) {
                            var seleniumRgx = new RegExp(config.selenium.jar),
                                engineRgx = new RegExp(config.slamengine.jar);
                            if (seleniumRgx.test(data)) {
                                log("Selenium launched");
                                launched.selenium = true;
                            }
                            if (engineRgx.test(data)) {
                                log("Slamengine launched");
                                launched.engine = true;
                            }
                            run();
                        });
                    }, 1000);
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

