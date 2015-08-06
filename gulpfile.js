"use strict";

var gulp = require("gulp"),
    purescript = require("gulp-purescript"),
    less = require("gulp-less"),
    webpack = require("webpack-stream"),
    run = require("gulp-run"),
    rimraf = require("rimraf");

var sources = [
  "src/**/*.purs",
  "bower_components/purescript-*/src/**/*.purs",
  "test/src/**/*.purs"
];

var foreigns = [
  "src/**/*.js",
  "bower_components/purescript-*/src/**/*.js",
  "test/src/**/*.js"
];

gulp.task("clean", function () {
  ["output", "tmp", "public/js/file.js", "public/js/notebook.js", "public/css/main.css"].forEach(function (path) {
    rimraf.sync(path);
  });
});

gulp.task("make", function() {
  return purescript.psc({
    src: sources,
    ffi: foreigns
  });
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
  mkBundleTask("file", "Entries.File"),
  mkBundleTask("notebook", "Entries.Notebook")
]);

gulp.task("less", function() {
  return gulp.src(["less/main.less"])
    .pipe(less({ paths: ["less/**/*.less"] }))
    .pipe(gulp.dest("public/css"));
});

// We don't use `mkBundleTask` here as there's no need to `webpack` - this
// bundle as it's going to be running in node anyway
gulp.task("bundle-test", ["bundle", "less"], function() {
  return purescript.pscBundle({
    src: "output/**/*.js",
    output: "tmp/js/test.js",
    module: "Test.Selenium",
    main: "Test.Selenium"
  });
});

gulp.task("test", ["bundle-test"], function() {
  return run("node test", { verbosity: 3 }).exec();
});

var mkWatch = function(name, target, files) {
  gulp.task(name, [target], function() {
    return gulp.watch(files, [target]);
  });
};

var allSources = sources.concat(foreigns);
mkWatch("watch-less", "less", ["less/**/*.less"]);
mkWatch("watch-file", "bundle-file", allSources);
mkWatch("watch-notebook", "bundle-notebook", allSources);

gulp.task("default", ["watch-less", "watch-file", "watch-notebook"]);
