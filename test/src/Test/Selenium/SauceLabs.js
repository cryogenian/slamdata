// module Test.Selenium.SauceLabs

exports.sauceCapabilities = function (config) {
  return { username: config.credentials.username
         , accessKey: config.credentials.accessKey
         , platform: config.platform
         , browserName: config.browserName
         };
};

