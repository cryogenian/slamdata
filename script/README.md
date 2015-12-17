To download a Quasar jar of the correct version, run `get-quasar.js` from the
project root  with a GitHub authorization token:

```sh
GITHUB_AUTH_TOKEN=XXXXXXXX node script/get-quasar.js
```

The jar will be placed at `jars/quasar.jar`, and will be used in the
integration tests.

To download Quasar Advanced, use the `--advanced` option:

```sh
GITHUB_AUTH_TOKEN=XXXXXXXX node script/get-quasar.js --advanced
```
