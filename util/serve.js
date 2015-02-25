var express = require("express"),
    gulp = require("gulp"),
    http = require("http"),
    ecstatic = require("ecstatic");

module.exports = function(taskName, serverPort, subdirectory) {
    if (taskName === undefined) {
        taskName = "serve";
    }
    if (serverPort === undefined) {
        serverPort = 5050;
    }
    if (subdirectory === undefined) {
        subdirectory = "public";
    }
    var app = express();
    app.use(express.static(__dirname + '/../' + subdirectory));
    app.use("/lib", express.static(__dirname + '/../bower_components'));

    app.post("/fileupload", function(req, res) {
        req.rawBody = '';
        req.setEncoding('utf8');
        
        req.on('data', function(chunk) {
            console.log("!!!");
            req.rawBody += chunk;
        });
        
        req.on('end', function() {
            console.log(req.rawBody);
            res.send("ok");
        });
    });
    return function() {
        app.listen(serverPort);
    };
};
