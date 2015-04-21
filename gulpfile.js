var mandragora = require("mandragora-bucket/index.js"),
    gulp = require("gulp"),
    less = require("gulp-less");


mandragora.config.entries = {
    "Entries.File": {
        "name": "file",
        "dir": "public/js"
    },
    "Entries.Notebook": {
        "name": "notebook",
        "dir": "public/js"
    }
};
mandragora(gulp);

gulp.task("less", function() {
    return gulp.src(["less/main.less"])
        .pipe(less({
            paths: ["less/**/*.less"]
        }))
        .pipe(gulp.dest("public/css"));


});

gulp.task("watch-less", ["less"], function() {
    return gulp.watch(["less/**/*.less"],
                      ["less"]);
});


