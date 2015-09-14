[![Latest release](https://img.shields.io/github/release/slamdata/slamdata.svg)](https://github.com/slamdata/slamdata/releases)
[![Travis build status](https://travis-ci.org/slamdata/slamdata.svg?branch=master)](https://travis-ci.org/slamdata/slamdata)
[![Coverage Status](https://coveralls.io/repos/slamdata/slamdata/badge.svg)](https://coveralls.io/r/slamdata/slamdata)
[![Join the chat at https://gitter.im/slamdata/slamdata](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/slamdata/slamdata?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

**Issues for this project are kindly hosted by [Atlassian JIRA](https://slamdata.atlassian.net). Signup is open to anyone, so if you want to contribute, have bugs to report or features to suggest, [sign up for a JIRA account](https://slamdata.atlassian.net).**

# SlamData

Web-based visual analytics for NoSQL data, powered by Quasar.

## Pre-built releases

The [GitHub releases page](https://github.com/slamdata/slamdata/releases) for the project contains pre-built archives.

See the [Use with Quasar](#use-with-quasar) section for next steps.

## Building from source

Building SlamData 2.1 requires [PureScript 0.7.1](https://github.com/purescript/purescript/releases/tag/v0.7.1) and Node.js v0.12.x.

#### Prerequisites

This can be skipped if [Bower](http://bower.io/) and [Gulp.js](http://gulpjs.com/) are already installed globally.

```
npm install bower gulp -g
```

#### Checkout

```
git clone git@github.com:slamdata/slamdata.git
```

#### Fetch dependencies

Inside the `slamdata` repository directory:

```
bower install
npm install
```

#### Build

```
gulp less bundle
```

After `gulp` finishes the `public` directory will contain the complete SlamData front-end app.

For working on SlamData, the default `gulp` task can be run instead. This will run an incremental build with a watch process so any changes to the `.less`, `.purs`, or `.js` source files will trigger a rebuild.

## Use with Quasar

Quasar can host the SlamData front-end app on the same port as the API. To do this, use the `--content-path` command-line argument when starting the engine:

```
java -jar [<path to quasar jar>] --content-path [<path to slamdata app>]
```

Where `[<path to slamdata app>]` is the location of the source-built `public` folder, or the extracted contents of a pre-built archive.
