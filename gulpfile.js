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
    sequence = require("run-sequence"),
    replace = require("gulp-replace"),
    footer = require("gulp-footer");

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

// Webdriver thinks elements which are obscured by elements with fixed positions
// are visible. By and large fixed positions fall back gracefully to
// absolute positions so this task can be used to swap the two after less
// compilation.
// This task can be reverted by the `less` task.
gulp.task('prevent-css-transitions-and-remove-fixed-positions', function() {
  var css = fs.readFileSync("test/prevent-css-transitions.css", "utf8");
  gulp.src(['public/css/main.css'])
    .pipe(footer(css))
    .pipe(replace('fixed', 'absolute'))
    .pipe(gulp.dest('public/css'));
});

gulp.task("clean", function () {
    [
        "output",
        "tmp",
        "public/js/file.js",
        "public/js/filesystem.js",
        "public/js/workspace.js",
        "public/js/auth_redirect.js",
        "public/css/main.css"
    ].forEach(function (path) {
        rimraf.sync(path);
    });
});

gulp.task("make", function() {
  return purescript.psc({
    src: testSources,
    ffi: foreigns
  });
});

gulp.task("add-headers", ["replace-headers"], function () {
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

/**
* To switch license file
* + Copy old license file to `headers` folder
* + Run `gulp add-headers`
* + Remove `headers` folder
**/
gulp.task("replace-headers", function() {
    var licenseHeader =
            "{-\n" + fs.readFileSync('LICENSE.header', 'utf8') + "-}";

    return (function() {
        // Extract old header files
        try {
            return fs.readdirSync("./headers").filter(function(name) {
                return /.*\.header/.test(name);
            });
        } catch (e) {
            // don't warn here, it's ok if `headers` folder is empty or doesn't exist
            return [];
        }
    }()).map(function(name) {
        // Read their content
        var file = "./headers/" + name;
        var fileContent;
        try {
            fileContent = "{-\n" + fs.readFileSync(file, "utf8") + "-}";
        } catch (e) {
            // warn here, we don't know maybe we must read this file but it has
            // incorrect permissions
            console.warn(e.message);
            fileContent = undefined;
        }
        return fileContent;
    }).filter(function(content) {
        // Filter empty files and files with incorrect permissions
        return typeof content !== "undefined";
    }).reduce(function(acc, content) {
        // Foldl gulp task
        return function() {
            return acc().pipe(replace(content, licenseHeader));
        };
    }, function() {
        // Initial gulp task
        return gulp.src(slamDataSources, {base: "./"});
        // run gulp task
    })().pipe(gulp.dest("./"));
});

gulp.task("replace-crlf", function() {
    gulp.src(slamDataSources, {base: "./"})
        .pipe(replace(/\r\n/g, "\n"))
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

  gulp.task("prebundle-" + name, function() {
    return purescript.pscBundle({
      src: "output/**/*.js",
      output: "tmp/" + name + ".js",
      module: main,
        main: main,
        optimize: ["uncurry"]
    });
  });

  gulp.task("bundle-" + name, ["prebundle-" + name], function () {
    return gulp.src("tmp/" + name + ".js")
          .pipe(webpack({
              resolve: {
                  modulesDirectories: ["node_modules"]

              },
              output: { filename: name + ".js" },
              module: {
                  loaders: [
                      {
                          include: /\.json$/,
                          loaders: ["json-loader"]

                      },
                      {
                          test: require.resolve("echarts"),
                          loader: "expose?echarts"
                      }
                  ]
              }
          })).pipe(gulp.dest("public/js"));
  });

  return "bundle-" + name;
};

gulp.task("bundle", [
  mkBundleTask("filesystem", "SlamData.FileSystem"),
  mkBundleTask("workspace", "SlamData.Workspace"),
  mkBundleTask("auth_redirect", "SlamData.AuthRedirect"),
]);

gulp.task("make-bundle", function () {
    sequence("make", "bundle");
});

gulp.task("bundle-test", ["bundle"], function() {
    sequence("less", "prevent-css-transitions-and-remove-fixed-positions", function() {
        return purescript.pscBundle({
            src: "output/**/*.js",
            output: "test/index.js",
            module: "Test.SlamData.Feature.Main",
            main: "Test.SlamData.Feature.Main"
        });
    });
});

gulp.task("bundle-property-tests", function() {
    return purescript.pscBundle({
      src: "output/**/*.js",
      output: "tmp/js/property-tests.js",
      module: "Test.SlamData.Property",
      main: "Test.SlamData.Property"
    });
});

var mkWatch = function(name, target, files) {
  gulp.task(name, [target], function() {
    return gulp.watch(files, [target]);
  });
};

var allSources = sources.concat(foreigns);
mkWatch("watch-file", "bundle-file", allSources);
mkWatch("watch-workspace", "bundle-workspace", allSources);
mkWatch("watch-auth_redirect", "bundle-auth_redirect", allSources);

gulp.task("less", function() {
  return gulp.src(["less/main.less"])
    .pipe(less({ paths: ["less/**/*.less"] }))
    .pipe(gulp.dest("public/css"));
});
mkWatch("watch-less", "less", ["less/**/*.less"]);

gulp.task("full", ["clean", "add-headers", "trim-whitespace", "less", "make-bundle"]);
gulp.task("default", ["less", "make-bundle"]);
