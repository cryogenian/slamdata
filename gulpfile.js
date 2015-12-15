"use strict";

var gulp = require("gulp"),
    header = require("gulp-header"),
    contentFilter = require("gulp-content-filter"),
    purescript = require("gulp-purescript"),
    webpack = require("webpack-stream"),
    rimraf = require("rimraf"),
    fs = require("fs"),
    trimlines = require("gulp-trimlines"),
    less = require("gulp-less"),
    sequence = require("run-sequence");

var slamDataSources = [
  "src/**/*.purs",
];

var vendorSources = [
  "bower_components/purescript-*/src/**/*.purs",
];

var sources = slamDataSources.concat(vendorSources);

var testSources = [
    "test/src/**/*.purs"
].concat(sources);

var foreigns = [
  "src/**/*.js",
  "bower_components/purescript-*/src/**/*.js",
  "test/src/**/*.js"
];

gulp.task("clean", function () {
    [
        "output",
        "tmp",
        "public/js/file.js",
        "public/js/filesystem.js",
        "public/js/notebook.js",
        "public/css/main.css"
    ].forEach(function (path) {
        rimraf.sync(path);
    });
});

gulp.task("make", function() {
  return purescript.psc({
    src: sources,
    ffi: foreigns
  });
});

gulp.task("test-make", function() {
    return purescript.psc({
        src: testSources,
        ffi: foreigns
    });
});

gulp.task("add-headers", function () {
  // read in the license header
  var licenseHeader = "{-\n" + fs.readFileSync('LICENSE.header', 'utf8') + "-}\n\n";

  // filter out files that already have a license header
  var contentFilterParams = { include: /^(?!\{-\nCopyright)/ };

  // prepend license header to all source files
  return gulp.src(slamDataSources, {base: "./"})
            .pipe(contentFilter(contentFilterParams))
            .pipe(header(licenseHeader))
            .pipe(gulp.dest("./"));
});

gulp.task("trim-whitespace", function () {
  var options = { leading: false };
  return gulp.src(slamDataSources, {base: "./"})
            .pipe(trimlines(options))
            .pipe(gulp.dest("."));
});

var bundleTasks = [];

var mkBundleTask = function (name, main) {

  gulp.task("prebundle-" + name, ["make"], function() {
    return purescript.pscBundle({
      src: "output/**/*.js",
      output: "tmp/js/" + name + ".js",
      module: main,
      main: main
    });
  });

  gulp.task("bundle-" + name, ["prebundle-" + name], function () {
    return gulp.src("tmp/js/" + name + ".js")
      .pipe(webpack({
        resolve: { modulesDirectories: ["node_modules"] },
        output: { filename: name + ".js" }
      }))
      .pipe(gulp.dest("public/js"));
  });

  return "bundle-" + name;
};

gulp.task("bundle", [
  mkBundleTask("filesystem", "Entry.FileSystem"),
  mkBundleTask("notebook", "Entry.Dashboard"),
]);

gulp.task("bundle-test", ["bundle"], function() {
    sequence("less", "test-make", function() {
        return purescript.pscBundle({
            src: "output/**/*.js",
            output: "tmp/js/test.js",
            module: "Test.Selenium",
            main: "Test.Selenium"
        });
    });

});


var mkWatch = function(name, target, files) {
  gulp.task(name, [target], function() {
    return gulp.watch(files, [target]);
  });
};

var allSources = sources.concat(foreigns);
mkWatch("watch-file", "bundle-file", allSources);
mkWatch("watch-notebook", "bundle-notebook", allSources);

gulp.task("less", function() {
  return gulp.src(["less/main.less"])
    .pipe(less({ paths: ["less/**/*.less"] }))
    .pipe(gulp.dest("public/css"));
});
mkWatch("watch-less", "less", ["less/**/*.less"]);

// gulp.task("default", ["add-headers", "trim-whitespace", "less", "bundle"]);
gulp.task("default", ["less", "bundle"]);
