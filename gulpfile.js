var mandragora = require("mandragora-bucket/index.js");
var gulp = require("gulp");
mandragora(gulp, {
    paths: {
        bower: [
            "bower_components/purescript-*/src/**/*.purs",
            "bower_components/purescript-*/purescript-*/src/**/*.purs"
        ],
        test: ["test/**/*.purs"],
        src: ["src/**/*.purs"]
    },
    tmpDir: "tmp",
    entries: {
        "Entries.File": {
            "name": "file",
            "dir": "public"
        },
        "Entries.Notebook": {
            "name": "notebook",
            "dir": "public"
        }
    },
    docs: "MODULES.md",
    tmpDir: "dist"
});


gulp.task("default", ["watch-main"]);
