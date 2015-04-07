var mandragora = require("mandragora-bucket/index.js"),
    gulp = require("gulp"),
    less = require("gulp-less");


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


gulp.task("less", function() {
    return gulp.src(["less/main.less"])
        .pipe(less({
            paths: ["less/**/*.less"]g
        }))
        .pipe(gulp.dest("public"));
           

});

gulp.task("watch-less", ["less"], function() {
    return gulp.watch(["less/**/*.less"],
                      ["less"]);
});


