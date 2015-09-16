# Tests

## prerequisites

* NodeJS to run tests
* Java to run `Quasar`
* MongoDb > 2.6
* imagemagick to take screenshots

## How to take screenshots (if they are changed, but absolutely correct)

* remove `test/image`
* set `collectingScreenshots` to `true` in `test/config.json`
* run tests (you can comment tests that don't check screenshots to gather them faster)
* don't hover charts during this test.
