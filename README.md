[![Latest release](https://img.shields.io/github/release/slamdata/slamdata.svg)](https://github.com/slamdata/slamdata/releases)
[![Travis build status](https://travis-ci.org/slamdata/slamdata.svg?branch=master)](https://travis-ci.org/slamdata/slamdata)
[![Coverage Status](https://coveralls.io/repos/slamdata/slamdata/badge.svg)](https://coveralls.io/r/slamdata/slamdata)
[![Join the chat at https://gitter.im/slamdata/slamdata](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/slamdata/slamdata?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

# SlamData

Web-based visual analytics for NoSQL data, powered by [Quasar](https://github.com/quasar-analytics/quasar).

See the [Use with Quasar](#use-with-quasar) section for next steps.

## Building from source

Building SlamData requires Node.js v4.1.0 or newer. The app is written in [PureScript](http://www.purescript.org/), the appropriate version of the PureScript compiler will be installed as part of the `npm` dependencies (see below).

#### Prerequisites

This can be skipped if [Bower](http://bower.io/) is already installed globally.

```
npm install bower -g
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
npm run build
```

After this task finishes the `public` directory will contain the complete SlamData front-end app.

## Use with Quasar

Quasar can host the SlamData front-end app on the same port as the API. To do this, use the `--content-path` command-line argument when starting the engine:

```
java -jar [<path to quasar jar>] --content-path [<path to slamdata app>]
```

Where `[<path to slamdata app>]` is the location of the source-built `public` folder, or the extracted contents of a pre-built archive.

## Custom styling

Users can add custom styles to notebooks by adding a query parameter to the URI.
For example, to add a stylesheet located in `css/foo.css` to
```
http://slamdata.instance.com/notebook.html#/db/Folder/Notebook.slam/view
```
one should modify the route to
```
http://slamdata.instance.com/notebook.html?stylesheets=css/foo.css#/db/Folder/Notebook.slam/view
```
The values of `stylesheets` are decoded and then split by `,`, so to add two stylesheets one could use

+ `stylesheets=css/foo.css,http%3A%2F%2Ffoo.com%2Fstyles.css`
+ `stylesheets=css%2Ffoo.css,http%3A%2F%2Ffoo.com%2Fstyles.css`
+ `stylesheets=css%2Ffoo.css%2Chttp%3A%2F%2Ffoo.com%2Fstyles.css`

These URIs are checked and, if they are valid, corresponding `link` elements are added to the `head`

Here it would be

```html
<link type="text/css" rel="stylesheet" href="css/foo.css">
<link type="text/css" rel="stylesheet" href="http://foo.com/style.css">
```
## Additional permissions

Working with Quasar Advanced, the user can add permission tokens to the URI, which will be sent with every ajax request.

For example to add permission tokens `ABCD` and `1234` to
```
http://slamdata.instance.com/notebook.html#/db/Folder/Notebook.slam/view
```
one should modify the route to
```
http://slamdata.instance.com/notebook.html?permissionsToken=ABCD,1234#/db/Folder/Notebook.slam/view
```

These tokens then will be added to `X-Extra-Permissions` header.

The value of `permissionsToken` can be url-encoded: `ABCD,1234` is equal with `ABCD%2C1234`.
