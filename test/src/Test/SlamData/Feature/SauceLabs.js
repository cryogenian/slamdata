// module Test.SlamData.Feature.SauceLabs

exports.sauceCapabilities = function (config) {
  return { username: config.credentials.username
         , accessKey: config.credentials.accessKey
         , platform: config.platform
         , browserName: config.browserName
         , tunnelIdentifier: config.tunnelIdentifier
         , maxDuration: config.maxDuration
         };
};

