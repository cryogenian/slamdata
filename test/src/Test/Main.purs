module Test.Main where

import Prelude

import Control.Monad (when)
import Control.Monad.Aff (Aff(), forkAff, runAff, launchAff, apathize, attempt, later', cancel)
import Control.Monad.Aff.AVar (makeVar, takeVar, putVar, killVar, AVAR())
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console as Ec
import Control.Monad.Eff.Exception (Error(), EXCEPTION(), throwException, error, message)
import Control.Monad.Eff.Ref (newRef, writeRef, modifyRef, readRef)
import Control.Monad.Error.Class (throwError)

import Data.Array as Arr
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Functor.Eff (liftEff)
import Data.Maybe (Maybe(..))
import Data.String as Str

import Database.Mongo.Mongo (connect, close)

import Node.ChildProcess (ChildProcess(), makeSpawnOption, stdout, stderr, spawn, kill, execSync)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS())
import Node.FS.Aff (unlink, mkdir)
import Node.Path (resolve)
import Node.Process as Process
import Node.Rimraf (rimraf)
import Node.Stream (Readable(), Duplex(), pipe, onDataString, onClose)

import Test.Config (Config())
import Test.Effects (TestEffects())
import Test.Selenium as Selenium
import Text.Chalky

foreign import getConfig :: forall e. Eff (fs :: FS|e) Config
foreign import createReadStream
  :: forall e. String -> Aff e (Readable () e)
foreign import createWriteStream
  :: forall e. String -> Aff e (Duplex e)
foreign import stack
  :: Error -> String

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
      -> Aff TestEffects (Readable r TestEffects))
  -> String
  -> Aff TestEffects ChildProcess
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

cleanMkDir :: String -> Aff TestEffects Unit
cleanMkDir path = do
  let p = resolve [path] ""
  rimraf p
  mkdir p

main :: Eff TestEffects Unit
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
                   $ Selenium.test rawConfig
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
