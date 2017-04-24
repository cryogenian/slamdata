"use strict";

const gulp = require("gulp");
const header = require("gulp-header");
const contentFilter = require("gulp-content-filter");
const rimraf = require("rimraf");
const fs = require("fs");
const trimlines = require("gulp-trimlines");
const less = require("gulp-less");
const sequence = require("run-sequence");
const replace = require("gulp-replace");
const footer = require("gulp-footer");
const path = require("path");
const exec = require("child_process").exec;
const webpack = require("webpack-stream");
const { injectIconsIntoHTML, createIconPureScript } = require("./script/icons")

const slamDataSources = [
  "src/**/*.purs",
];

const vendorSources = [
  "bower_components/purescript-*/src/**/*.purs",
];

const sources = slamDataSources.concat(vendorSources);

const testSources = [
  "test/src/**/*.purs"
].concat(sources);

const foreigns = [
  "src/**/*.js",
  "bower_components/purescript-*/src/**/*.js",
  "test/src/**/*.js"
];

function pursBundle(name, main) {
  return function(done) {
    const cmd = [
      'purs bundle "output/**/*.js"',
      '--output', name + '.js',
      '--main', main,
      '--module', main
    ].join(' ');

    exec(cmd, function(stderr, stdout) {
      if (stderr) {
        done(stderr);
      } else {
        done();
      }
    });
  };
}

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


var mkBundleTask = function (name, main) {
  gulp.task("prebundle-" + name, pursBundle("tmp/" + name, main));
  gulp.task("bundle-" + name, ["prebundle-" + name], function () {
    return gulp.src("tmp/" + name + ".js")
          .pipe(webpack({
              resolve: {
                  modulesDirectories: ["node_modules"],
                  alias: {
                      "package.json": path.join(__dirname, "package.json")
                  }
              },
            output: { filename: name + ".js" },
            plugins: [
              new webpack.webpack.optimize.UglifyJsPlugin({
                comments: false,
                compress: {
                  unused: true,
                  dead_code: true,
                  warnings: false,
                  drop_debugger: true,
                  conditionals: true,
                  evaluate: true,
                  drop_console: true,
                  sequences: true,
                  booleans: true,
                },
                sourceMap: false
              }),
            ],
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

gulp.task("bundle-test", ["prevent-css-transitions-and-remove-fixed-positions"],
  pursBundle("test/index", "Test.SlamData.Feature.Main"));

gulp.task("bundle-property-tests",
  pursBundle("tmp/js/property-tests", "Test.SlamData.Property"));

var mkWatch = function(name, target, files) {
  gulp.task(name, [target], function() {
    return gulp.watch(files, [target]);
  });
};

var allSources = sources.concat(foreigns);
mkWatch("watch-file", "bundle-file", allSources);
mkWatch("watch-workspace", "bundle-workspace", allSources);
mkWatch("watch-auth_redirect", "bundle-auth_redirect", allSources);

gulp.task("inject-icons", injectIconsIntoHTML);
gulp.task("icon-purs", createIconPureScript);
gulp.task("icons", [ "inject-icons", "icon-purs" ]);
gulp.task("full", [ "add-headers", "trim-whitespace" ]);
