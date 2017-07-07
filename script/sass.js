"use strict"

const autoprefixer = require("gulp-autoprefixer")
const cleanCSS = require("gulp-clean-css")
const gulp = require("gulp")
const sass = require("gulp-sass")


const sass_ =
  gulp.src("./sass/themes/*.sass")
    .pipe(sass())
    .pipe(autoprefixer({ browers: "> 2%, last 1 versions, Firefox ESR, not Explorer < 11, not Android < 4.4" }))
    .pipe(cleanCSS({ advanced: true, level: 2 }))
    .pipe(gulp.dest("public/css/"))

module.exports = {
  sass: sass_
}
