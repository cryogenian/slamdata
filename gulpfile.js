"use strict"

var gulp = require("gulp"),
    purescript = require("gulp-purescript"),
    less = require("gulp-less"),

    browserify = require('browserify'),
    watchify = require('watchify'),
    plumber = require('gulp-plumber'),
    gutil = require('gulp-util'),
    source = require('vinyl-source-stream'),
    buffer = require('vinyl-buffer'),
    chalk = require('chalk'),
    watch = require('gulp-watch');


var sources = [
    "src/**/*.purs",
    "bower_components/purescript-*/src/**/*.purs"
];

var foreigns = [
    "src/**/*.js",
    "bower_components/purescript-*/src/**/*.js"
];

var fileBundler = watchify(browserify({
    entries: ['entries/file.js'],
    paths: ["output", "node_modules"],
    cache: {},
    packageCache: {}
}));

var notebookBundler =  watchify(browserify({
    entries: ['entries/notebook.js'],
    paths: ["output", "node_modules"],
    cache: {},
    packageCache: {}
}));

function bundleFile() {
    var filename = chalk.magenta('file.js');
    gutil.log("Browserify bundling " + filename + "...");
    return fileBundler.bundle()
        .on("error", gutil.log.bind(gutil, "Browserify error bundling " + filename))
        .pipe(source('file.js'))
        .pipe(gulp.dest("public/js"))
        .on("end", gutil.log.bind(gutil, "Browserify bundle " + filename + " updated"));
}

function bundleNotebook() {
    var filename = chalk.magenta('notebook.js');
    gutil.log("Browserify bundling " + filename + "...");
    return notebookBundler.bundle()
        .on("error", gutil.log.bind(gutil, "Browserify error bundling " + filename))
        .pipe(source('notebook.js'))
        .pipe(gulp.dest("public/js"))
        .on("end", gutil.log.bind(gutil, "Browserify bundle " + filename + " updated"));
}



gulp.task('bundle-file', ['make'], bundleFile);

gulp.task('bundle-notebook', ['make'], bundleNotebook);

gulp.task('watch-file', ['bundle-file'], function() {
    watch(sources.concat(foreigns), function() {
        gulp.start('bundle-file');
    });
});

gulp.task('watch-notebook', ['bundle-notebook'], function() {
    watch(sources.concat(foreigns), function() {
        gulp.start('bundle-notebook');
    });
});

gulp.task('bundle', ['bundle-file', 'bundle-notebook'], function() {
    process.exit(0);
});

gulp.task("make", function() {
    return purescript.psc({
        src: sources,
        ffi: foreigns
    });
});

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

gulp.task('default', ['watch-less', 'watch-file', 'watch-notebook']);
