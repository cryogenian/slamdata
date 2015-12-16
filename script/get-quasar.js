var https = require("https");
var fs = require("fs");
var quasar = require("../quasar-versions.json");

function githubRequestHeaders(options) {
  var requestHeaders =
    { "User-Agent": "GitHubAPI"
    , "Authorization": "token " + options.authorizationToken
    };

  return requestHeaders;
};

function fetchReleaseInfo(options, k) {
  var requestOptions =
    { host: "api.github.com"
    , headers: githubRequestHeaders(options)
    , path: "/repos/" + options.owner + "/" + options.repo + "/releases/tags/" + options.tag
    };

  https.get(requestOptions, function (response) {
    var body = "";

    response.on("data", function (chunk) {
      body += chunk;
    });

    response.on("end", function () {
      k(JSON.parse(body));
    });
  });
}

function fetchReleaseAsset(options, destFile, k) {
  var requestHeaders = githubRequestHeaders(options);
  requestHeaders["Accept"] = "application/octet-stream";

  var requestOptions =
    { host: "api.github.com"
    , headers: requestHeaders
    , path: "/repos/" + options.owner + "/" + options.repo + "/releases/assets/" + options.assetId
    };

  https.get(requestOptions, function (response) {
    var assetLocation = response.headers.location;
    if (assetLocation == undefined) {
      throw "Asset location not found";
    };

    https.get(assetLocation, function (assetResponse) {
      assetResponse.pipe(destFile);
      destFile.on("finish", k);
    });
  });
};

function downloadQuasar(quasarOptions, token, destFile, k) {
  quasarOptions.authorizationToken = token;

  fetchReleaseInfo(quasarOptions, function (info) {
    var assets =
      info.assets.filter(function (asset) {
        return asset.name.indexOf("web") === 0
      });

    var asset = assets[0];
    if (asset == undefined) {
      throw "No JAR asset found";
    }

    var assetOptions =
      { owner: quasarOptions.owner
      , repo: quasarOptions.repo
      , assetId: asset.id
      , authorizationToken: token
      };

    fetchReleaseAsset(assetOptions, destFile, k);
  });
}

function main() {
  var githubAuthToken = process.env.GITHUB_AUTH_TOKEN

  if (githubAuthToken == undefined) {
    console.warn("[Warning] Quasar jar will not be fetched unless you set the GITHUB_AUTH_TOKEN environment variable");
  } else {

    if (!fs.existsSync("./jars")){
      fs.mkdirSync("./jars");
    }

    var quasarDest = fs.createWriteStream("./jars/quasar.jar");

    var quasarDistribution = "quasar";
    if (process.argv.indexOf("--advanced") != -1) {
      quasarDistribution = "quasar-advanced";
    }

    downloadQuasar(quasar[quasarDistribution], githubAuthToken, quasarDest, function () {
      quasarDest.close(function () {
        console.log("Downloaded " + quasarDistribution);
      });
    });
  }
}

main();
