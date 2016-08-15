module.exports = {
  mongodb: {
    host: "localhost",
    port: 63174
  },
  restoreCmd: "mongorestore --host 127.0.0.1 -d testDb --port 63174 --drop test/dump/demo",
  selenium: {
    waitTime: 30000
  },
  slamdataUrl: "http://localhost:63175",
  quasar: {
    jar: "jars/quasar.jar",
    config: "test/quasar-config.json"
  },
  database: {
    name: "testDb"
  },
  upload: {
    filePaths: ["./test/line-delimited.json", "./test/array-wrapped.json"]
  },
  download: {
    folder: "tmp/test/downloads"
  }
}
