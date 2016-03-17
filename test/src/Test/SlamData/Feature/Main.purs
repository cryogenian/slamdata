module Test.SlamData.Feature.Main where

import Control.Monad (when)
import Control.Monad.Aff
  (Aff(), forkAff, runAff, launchAff, apathize, attempt, later', cancel)
import Control.Monad.Aff.AVar
  (makeVar, takeVar, putVar, killVar, AVAR())
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE())
import Control.Monad.Eff.Console as Ec
import Control.Monad.Eff.Exception
  (Error(), EXCEPTION(), throwException, error, message)
import Control.Monad.Eff.Ref
  (REF(), newRef, writeRef, modifyRef, readRef)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader.Trans
import DOM (DOM())
import Data.Array as Arr
import Data.Either (Either(..), either)
import Data.Foldable (traverse_)
import Data.Functor.Eff (liftEff)
import Data.Maybe (Maybe(..))
import Data.Maybe (maybe, isJust)
import Data.Monoid (mempty)
import Data.String as Str
import Database.Mongo.Mongo (connect, close)
import Graphics.EasyImage (EASY_IMAGE())
import Graphics.ImageDiff (IMAGE_MAGICK())
import Node.ChildProcess
  (ChildProcess(), makeSpawnOption, stdout, stderr, spawn, kill, execSync)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS())
import Node.FS.Aff (unlink, mkdir)
import Node.Path (resolve)
import Node.Process as Process
import Node.Rimraf (rimraf)
import Node.Stream (Readable(), Duplex(), pipe, onDataString, onClose)
import Platform (PLATFORM())
import Prelude
import Selenium (setFileDetector, quit)
import Selenium.Browser
import Selenium.Builder
import Selenium.Capabilities
import Selenium.FFProfile
import Selenium.Monad (setWindowSize)
import Selenium.Remote as SR
import Selenium.Types (SELENIUM())
import Test.Feature.Monad (FeatureEffects())
import Test.SlamData.Feature.Config (Config())
import Test.SlamData.Feature.Effects (SlamFeatureEffects())
import Test.SlamData.Feature.File as File
import Test.SlamData.Feature.Notebook as Notebook
import Test.SlamData.Feature.Notebook as Notebook
import Test.SlamData.Feature.Notebook.Interactions
  (launchSlamData, mountTestDatabase)
import Test.SlamData.Feature.SauceLabs as SL
import Text.Chalky

foreign import getConfig :: forall e. Eff (fs :: FS|e) Config
foreign import createReadStream
  :: forall e. String -> Aff e (Readable () e)
foreign import createWriteStream
  :: forall e. String -> Aff e (Duplex e)
foreign import stack
  :: Error -> String

type Effects =
  SlamFeatureEffects (FeatureEffects
    ( ref :: REF
    , console :: CONSOLE
    , dom :: DOM
    , selenium :: SELENIUM
    ))

makeDownloadCapabilities :: Browser -> String -> Aff Effects Capabilities
makeDownloadCapabilities FireFox path = buildFFProfile do
  setIntPreference "browser.download.folderList" 2
  setBoolPreference "browser.download.manager.showWhenStarting" false
  setBoolPreference "browser.download.manager.focusWhenStartin" false
  setBoolPreference "browser.download.useDownloadDir" true
  setStringPreference "browser.download.dir" path
  setBoolPreference "browser.download.manager.closeWhenDone" true
  setBoolPreference "browser.download.manager.showAlertOnComplete" false
  setBoolPreference "browser.download.manager.useWindow" false
  setStringPreference "browser.helperApps.neverAsk.saveToDisk" "text/csv, application/ldjson"
makeDownloadCapabilities _ _ = mempty

test :: Config -> Aff Effects Unit
test config =
  maybe error go $ str2browser config.selenium.browser
  where
  error = void $ log $ red "Incorrect browser"
  go br = do
    log $ yellow $ config.selenium.browser <> " set as browser for tests\n\n"
    msauceConfig <- liftEff $ SL.sauceLabsConfigFromConfig config
    downloadCapabilities <-
      makeDownloadCapabilities br config.download.folder
    driver <- build $ do
      browser br
      traverse_ SL.buildSauceLabs msauceConfig
      withCapabilities downloadCapabilities


    when (isJust msauceConfig) $ do
      void $ log $ yellow $ "set up to run on Sauce Labs"
      (liftEff SR.fileDetector) >>= setFileDetector driver

    res <- attempt $ flip runReaderT { config: config
                                     , defaultTimeout: config.selenium.waitTime
                                     , driver: driver} do
      setWindowSize { height: 1280, width: 1024 }
      launchSlamData
      mountTestDatabase
--      File.test
      Notebook.test
    quit driver
    either throwError (const $ pure unit) res

copyFile
  :: forall e
   . String
  -> String
  -> Aff (fs :: FS, avar :: AVAR, err :: EXCEPTION|e) Unit
copyFile source tgt = do
  apathize $ unlink to
  readFrom <- createReadStream from
  writeTo <- createWriteStream to
  knot <- makeVar
  liftEff $ onClose writeTo $ runAff
     (const $ pure unit)
     (const $ pure unit)
     (putVar knot unit)
  liftEff $ readFrom `pipe` writeTo
  takeVar knot
  where
  from = resolve [source] ""
  to = resolve [tgt] ""

startProc
  :: forall r
  . String -> String -> Array String
  -> (ChildProcess
      -> Aff Effects (Readable r Effects))
  -> String
  -> Aff Effects ChildProcess
startProc name command args streamGetter check = do
  a <- makeVar
  started <- liftEff $ newRef false
  cancelKill <- forkAff $ later' 10000
                $ killVar a $ error $ name <> " has not been started"
  pr <- spawn command args \err -> do
    case err of
      Just e -> throwException e
      Nothing -> liftEff $ readRef started >>= \isStarted ->
        if not isStarted
        then throwException $ error $ name <> " process ended before it started"
        else pure unit

  stream <- streamGetter pr
  liftEff $ onDataString stream UTF8 \str -> do
    when (Str.contains check str) do
      liftEff $ writeRef started true
      launchAff $ putVar a pr

  result <- takeVar a
  cancel cancelKill $ error "Ok"
  log $ name <> " launched"
  pure result

mongoArgs :: Config -> Array String
mongoArgs config =
  [ "--port", show config.mongodb.port
  , "--dbpath", "tmp/data"
  ]

quasarArgs :: Config -> Array String
quasarArgs config =
  [ "-jar", resolve [config.quasar.jar] ""
  , "-c", resolve ["tmp", config.quasar.config] ""
  , "-C", resolve ["public"] ""
  , "-L", resolve ["public"] ""
  ]

seleniumArgs :: Config -> Array String
seleniumArgs config =
  [ "-jar", resolve [config.selenium.jar] "" ]

restoreCmd :: Config -> String
restoreCmd = _.restoreCmd

mongoConnectionString :: Config -> String
mongoConnectionString config =
  "mongodb://"
  <> config.mongodb.host
  <> ":" <> show config.mongodb.port
  <> "/" <> config.database.name

cleanMkDir :: String -> Aff Effects Unit
cleanMkDir path = do
  let p = resolve [path] ""
  rimraf p
  mkdir p

main :: Eff Effects Unit
main = do
  procs <- newRef []
  Process.onExit \_ ->
    readRef procs >>= traverse_ kill
  rawConfig <- getConfig
  runAff errHandler  (const $ Process.exit 0) do
    log $ gray "Creating data folder for MongoDB"
    cleanMkDir "tmp/data"
    log $ gray "Empting test folder"
    cleanMkDir "tmp/test"
    cleanMkDir "tmp/test/image"
    cleanMkDir "tmp/test/downloads"
    copyFile
      "test/quasar-config.json"
      "tmp/test/quasar-config.json"

    mongo <- startProc "MongoDB" "mongod" (mongoArgs rawConfig) stdout
             "waiting for connections on port"
    liftEff $ modifyRef procs (Arr.cons mongo)

    quasar <- startProc "Quasar" "java" (quasarArgs rawConfig) stdout
              "Server started listening on port"
    liftEff $ modifyRef procs (Arr.cons quasar)

    selenium <- startProc "Selenium" "java" (seleniumArgs rawConfig) stderr
                "Selenium Server is up and running"
    liftEff $ modifyRef procs (Arr.cons selenium)

    log $ gray "Restoring database"
    execSync (restoreCmd rawConfig)
      $ makeSpawnOption { stdio: ["pipe", "ignore", "pipe"]}
    log $ gray "Database restored"

    log $ magenta "Connecting database"
    db <- connect $ mongoConnectionString rawConfig
    log $ magenta "Ok, connected"


    log $ yellow "Starting tests"
    rawConfig <- liftEff getConfig
    testResults <- attempt
                   $ test rawConfig
                     { download = rawConfig.download
                       { folder = resolve [ rawConfig.download.folder ] "" }
                     , upload = rawConfig.upload
                       { filePaths =
                          map (\x -> resolve [ x ] "") rawConfig.upload.filePaths }
                     }
    close db
    case testResults of
      Left e ->  throwError e
      Right _ -> log $ green "OK, tests are passed"
  where
  errHandler e = do
    Ec.log $ red $ message e
    traverse_ (Ec.log <<< red) $ Str.split "\n" $ stack e
    Process.exit 1
