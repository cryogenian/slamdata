module Test.SlamData.Feature.Main where

import SlamData.Prelude

import Control.Monad.Aff (Aff, launchAff, runAff, apathize, attempt)
import Control.Monad.Aff.AVar (makeVar, takeVar, putVar, AVAR)
import Control.Monad.Aff.Console (log)
import Control.Monad.Aff.Reattempt (reattempt)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console as Ec
import Control.Monad.Eff.Exception (Error, EXCEPTION, message)
import Control.Monad.Eff.Ref (REF, newRef, modifyRef, readRef)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader.Trans (runReaderT)

import Data.Array as Arr
import Data.Posix.Signal (Signal(SIGTERM))
import Data.String as Str

import Database.Mongo.Mongo (connect, close)

import DOM (DOM)

import Node.ChildProcess as CP
import Node.FS (FS)
import Node.FS.Aff (unlink, mkdir, chmod)
import Node.FS.Perms as Np
import Node.Path (resolve)
import Node.Process as Process
import Node.Rimraf (rimraf)
import Node.Stream (Readable, Duplex, pipe, onClose)

import Platform (getPlatform, runOs, runPlatform)

import Quasar.Spawn.Util.Starter (starter, expectStdOut, expectStdErr)

import Selenium (quit)
import Selenium.Browser (Browser(..), browserCapabilities)
import Selenium.Builder (withCapabilities, build, usingServer)
import Selenium.Monad (setWindowSize)
import Selenium.Types (SELENIUM)

import Test.Feature.Monad (FeatureEffects)
import Test.SlamData.Feature.Config (Config)
import Test.SlamData.Feature.Effects (SlamFeatureEffects)
import Test.SlamData.Feature.Interactions (launchSlamData, mountTestDatabase)
import Test.SlamData.Feature.Monad (SlamFeature)
import Test.SlamData.Feature.Test.File as File
import Test.SlamData.Feature.Test.FlexibleVisualation as FlexibleVisualization
import Test.SlamData.Feature.Test.Markdown as Markdown
import Test.SlamData.Feature.Test.Search as Search
import Test.SlamData.Feature.Test.SaveCard as Save
import Test.SlamData.Feature.Test.FlipDeck as FlipDeck
import Text.Chalky (green, yellow, magenta, gray, red)

foreign import getConfig ∷ ∀ e. Eff (fs ∷ FS|e) Config
foreign import createReadStream
  ∷ ∀ e. String → Aff e (Readable () e)
foreign import createWriteStream
  ∷ ∀ e. String → Aff e (Duplex e)
foreign import stack
  ∷ Error -> String

type Effects =
  SlamFeatureEffects (FeatureEffects
    ( ref ∷ REF
    , console ∷ Ec.CONSOLE
    , dom ∷ DOM
    , selenium ∷ SELENIUM
    ))

tests ∷ SlamFeature Unit
tests = do
  launchSlamData
  mountTestDatabase

  File.test
  Search.test
  Markdown.test
  FlexibleVisualization.test
  Save.test
  FlipDeck.test

runTests ∷ Config → Aff Effects Unit
runTests config = do
  driver ← build do
    usingServer "http://127.0.0.1:4444/wd/hub"
    withCapabilities $ browserCapabilities Chrome

  let
    defaultTimeout = config.selenium.waitTime
    readerInp = { config, defaultTimeout, driver }

  res ← attempt $ flip runReaderT readerInp do
    setWindowSize { height: 800, width: 1024 }
    tests

  apathize $ quit driver
  either throwError (const $ pure unit) res

copyFile
  ∷ ∀ e
   . String
   → String
   → Aff (fs ∷ FS, avar ∷ AVAR, err ∷ EXCEPTION|e) Unit
copyFile source tgt = do
  apathize $ unlink to
  readFrom ← createReadStream from
  writeTo ← createWriteStream to
  knot ← makeVar

  liftEff
    $ onClose writeTo
    $ runAff
        (const $ pure unit)
        (const $ pure unit)
        (putVar knot unit)

  liftEff $ readFrom `pipe` writeTo
  takeVar knot
  where
  from = resolve [source] ""
  to = resolve [tgt] ""

procStartMaxTimeout ∷ Int
procStartMaxTimeout = 60000

startProc
  ∷ String
  → String
  → Array String
  → (Either String String → Maybe (Either String Unit))
  → Aff Effects CP.ChildProcess
startProc name command args check
  = reattempt procStartMaxTimeout
  $ starter name check
  $ liftEff
  $ CP.spawn command args CP.defaultSpawnOptions

mongoArgs ∷ Config → Array String
mongoArgs config =
  [ "--port", show config.mongodb.port
  , "--dbpath", "tmp/data"
  ]

quasarArgs ∷ Config → Array String
quasarArgs config =
  [ "-jar", resolve [config.quasar.jar] ""
  , "-c", resolve ["tmp", config.quasar.config] ""
  , "-C", resolve ["public"] ""
  , "-L", "/slamdata"
  ]

seleniumArgs ∷ Config → Array String
seleniumArgs config =
  [ "-jar", resolve [config.selenium.jar] ""
  , "-port", "4444"
  ]

mongoConnectionString ∷ Config → String
mongoConnectionString config =
  "mongodb://"
  ⊕ config.mongodb.host
  ⊕ ":" ⊕ show config.mongodb.port
  ⊕ "/" ⊕ config.database.name

cleanMkDir ∷ String → Aff Effects Unit
cleanMkDir path = do
  let p = resolve [path] ""
  rimraf p
  mkdir p

restoreDatabase ∷ Config → Aff Effects Unit
restoreDatabase rawConfig = do
  var ← makeVar
  liftEff $ CP.exec
    rawConfig.restoreCmd
    CP.defaultExecOptions
    (launchAff <<< putVar var)
  res ← takeVar var
  traverse_ throwError res.error


chromeDriverForOS ∷ String → Maybe String
chromeDriverForOS "win32" = Just "win.exe"
chromeDriverForOS "darwin" = Just "mac"
chromeDriverForOS "linux" = Just "linux"
chromeDriverForOS _ = Nothing

exeSuffix ∷ String → String
exeSuffix "win.exe" = ".exe"
exeSuffix _ = ""

copyChromeDriver ∷ Aff Effects Unit
copyChromeDriver = do
  platform ← getPlatform
  let
    chromeDriverName =
      platform
      >>= runPlatform
      >>> _.os
      >>> runOs
      >>> _.family
      <#> Str.toLower
      >>= chromeDriverForOS
  for_ chromeDriverName \driverName →
    let
      suffix = exeSuffix driverName
      oldName = "test/chromedriver/" ⊕ driverName
      newName = "chromedriver" ⊕ suffix
      allPerms = Np.mkPerms Np.all Np.all Np.all
    in
      copyFile oldName newName
      *> chmod newName allPerms


main ∷ Eff Effects Unit
main = do
  procs ← newRef []

  Process.onExit \_ → readRef procs >>= traverse_ (CP.kill SIGTERM)

  rawConfig ← getConfig

  runAff errHandler (const $ Process.exit 0) do
    log $ gray "Creating data folder for MongoDB"
    cleanMkDir "tmp/data"
    log $ gray "Empting test folder"
    cleanMkDir "tmp/test"
    cleanMkDir "tmp/test/image"
    cleanMkDir "tmp/test/downloads"
    copyFile
      "test/quasar-config.json"
      "tmp/test/quasar-config.json"


    log $ gray "Copying chromedriver"
    copyChromeDriver
    log $ gray "Ok, chromedriver is copied"

    mongo ←
      startProc
        "MongoDB"
        "mongod"
        (mongoArgs rawConfig)
        (expectStdOut "waiting for connections on port")
    liftEff $ modifyRef procs (Arr.cons mongo)

    quasar ←
      startProc
        "Quasar"
        "java"
        (quasarArgs rawConfig)
        (expectStdOut "Server started listening on port")
    liftEff $ modifyRef procs (Arr.cons quasar)

    selenium ←
      startProc
        "Selenium"
        "java"
        (seleniumArgs rawConfig)
        (expectStdErr "Selenium Server is up and running")
    liftEff $ modifyRef procs (Arr.cons selenium)

    log $ gray "Restoring database"
    restoreDatabase rawConfig
    log $ gray "Database restored"

    log $ magenta "Connecting database"
    db ← connect $ mongoConnectionString rawConfig
    log $ magenta "Ok, connected"

    log $ yellow "Starting tests"

    testResults
      ← attempt
        $ runTests rawConfig
            { download = rawConfig.download
              { folder = resolve [ rawConfig.download.folder ] "" }
              , upload = rawConfig.upload
                  { filePaths =
                       map (\x → resolve [ x ] "") rawConfig.upload.filePaths }
              }
    close db
    case testResults of
      Left e →  throwError e
      Right _ → log $ green "OK, tests are passed"

  where
  errHandler e = do
    Ec.log $ red $ message e
    traverse_ (Ec.log ∘ red) $ Str.split "\n" $ stack e
    Process.exit 1
