var Promise = require("bluebird"),
    childProcess = require("child_process"),
    mongodb = Promise.promisifyAll(require("mongodb")),
    mongo = mongodb.MongoClient,
    config = require("./config.json"),
    fs = require("fs"),
    path = require("path"),
    minimist = require("minimist"),
    chalk = require("chalk"),
    rimraf = require("rimraf");

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

function copyFile(from, to, kont) {
  var readStream = fs.createReadStream(from);
  var writeStream = fs.createWriteStream(to);

  readStream.pipe(writeStream);
  writeStream.on('close', kont);
}

// required for decodeURIComponent in Match.Route
window = global;
if (args.b) {
    config.selenium.browser = args.b;
}

if (args.remote) {
    config.sauceLabs.enabled = true;
}

var VERBOSE = args.v;

var quasarConfigPath = path.resolve("tmp/" + config.quasar.config);


var url = "mongodb://" + config.mongodb.host + ":" + config.mongodb.port + "/" + config.database.name,
    restoreCmd = config.restoreCmd,
    quasarArgs = [
        "-jar", path.resolve(config.quasar.jar),
        "-c", quasarConfigPath,
        "-C", "./slamdata",
        "-L", "/slamdata"
    ],
    seleniumArgs = ["-jar", path.resolve(config.selenium.jar)],
    mongoArgs = ["--port", config.mongodb.port, "--dbpath", "data"];

var procs = [];

var startProc = function (name, procName, args, stream, test) {
    return function () {
        return new Promise(function (resolve, reject) {
            var started = false;
            var proc = childProcess.spawn(procName, args, {}, function(err) {
                if (err) reject(err);
                if (!started) reject(new Error(name + " process ended before it started"));
                clearTimeout(timeout);
            });
            procs.push(proc);
            if (VERBOSE) {
                var prefix = name + ": ";
                proc.stdout.on("data", function(data) {
                    log(chalk.cyan(prefix + data.toString().split("\n").join("\n" + prefix)));
                });
                proc.stderr.on("data", function(data) {
                    log(chalk.yellow(prefix + data.toString().split("\n").join("\n" + prefix)));
                });
            }
            proc[stream].on("data", function(data) {
                if (data.toString().indexOf(test) != -1) {
                    log(name + " launched");
                    clearTimeout(timeout);
                    resolve(proc);
                    started = true;
                }
            });
            var timeout = setTimeout(function () {
                reject(new Error("Starting " + name + " timed out"));
            }, 10000);
        });
    };
};

var startMongo, startQuasar, startSelenium;

startQuasar = startProc("Quasar", "java", quasarArgs, "stdout", "Server started listening on port");

startMongo = startProc("MongoDB", "mongod", mongoArgs, "stdout", "waiting for connections on port");
startSelenium = startProc("Selenium", "java", seleniumArgs, "stderr", "Selenium Server is up and running");

function cleanMkDir(path) {
    try { rimraf.sync(path); } finally { }
    fs.mkdirSync(path);
}

log("Emptying test temp folder");
cleanMkDir("tmp/test");
cleanMkDir("tmp/test/image");
cleanMkDir("tmp/test/downloads");
config.download.folder = path.resolve("tmp/test/downloads/");

// Copy the configuration file for use by quasar
copyFile(path.resolve(config.quasar.config), quasarConfigPath, function(){});

process.chdir("tmp/test");

log("Creating data folder for MongoDB");
fs.mkdirSync("data");

log("Creating symlink for SlamData static files");
fs.symlinkSync(path.resolve("../../public"), path.resolve("slamdata"), "junction");

Promise.all([startMongo(), startQuasar(), startSelenium()])
    .then(function () {
        log("restoring test database");
        childProcess.execSync(restoreCmd, { stdio: ["pipe", "ignore", "pipe"] });
        log("database restored");
        return mongo.connect(url);
    }).then(function(db) {
        log("running tests\n\n");
        require("../tmp/js/test.js").test(config)(function() {
            db.close();
            process.exit(0);
        }, function(err) {
            log(err, err.stack.split("\n"));
            process.exit(1);
        });
    });

process.on("exit", function(code) {
  procs.forEach(function (proc) {
    proc.kill();
  });
});
