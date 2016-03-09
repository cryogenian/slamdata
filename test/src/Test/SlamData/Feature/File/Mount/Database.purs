{-
Copyright 2015 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Test.SlamData.Feature.File.Mount.Database where

import Prelude

import Control.Bind ((=<<))

import Selenium.ActionSequence hiding (sequence)
import Selenium.Combinators (checker, awaitUrlChanged, tryToFind)
import Selenium.Monad
import Selenium.Types

import Test.Feature.ActionSequence
import Test.SlamData.Feature.Common
import Test.Feature.Log
import Test.Feature.Monad (getModifierKey)
import Test.SlamData.Feature.Monad
import Test.SlamData.Feature.File.Common

findTestDb :: SlamFeature Element
findTestDb = do
  config <- getConfig
  findItem config.database.name

type MountConfigR =
  { host :: String
  , port :: Int
  }

mountConfigFromConfig :: SlamFeature MountConfigR
mountConfigFromConfig = do
  config <- getConfig
  pure { host : config.mongodb.host
       , port : config.mongodb.port
       }

mountDatabaseWithMountConfig :: MountConfigR -> SlamFeature Unit
mountDatabaseWithMountConfig mountConfig = do
  home
  getMountDatabaseButton >>= click'
  waitModalShown
  fieldByField

  where
  fieldByField :: SlamFeature Unit
  fieldByField = tryRepeatedlyTo do
    config <- getConfig
    nameField <- getNameField
    typeDropdown <- getTypeDropdown
    sequence do
      leftClick nameField
      keys config.mount.name
      leftClick typeDropdown
      keys "mongodb"
      sendEnter
    portField <- getPortField
    hostField <- getHostField
    pathField <- getPathField
    saveButton <- getSaveButton
    sequence do
      leftClick portField
      keys $ show mountConfig.port
      leftClick hostField
      keys mountConfig.host
      leftClick pathField
      keys config.database.name
      leftClick saveButton

  getSaveButton :: SlamFeature Element
  getSaveButton = do
    config <- getConfig
    tryRepeatedlyTo $ byCss config.configureMount.saveButton >>= findExact

  getUriField :: SlamFeature Element
  getUriField = do
    config <- getConfig
    tryRepeatedlyTo $ byCss config.configureMount.uriField >>= findExact

  getNameField :: SlamFeature Element
  getNameField = do
    config <- getConfig
    tryRepeatedlyTo $ byCss config.configureMount.nameField >>= findExact

  getTypeDropdown :: SlamFeature Element
  getTypeDropdown = do
    config <- getConfig
    tryRepeatedlyTo $ byCss config.configureMount.typeDropdown >>= findExact

  getPathField :: SlamFeature Element
  getPathField = do
    config <- getConfig
    tryRepeatedlyTo $ byCss config.configureMount.pathField >>= findExact

  getPortField :: SlamFeature Element
  getPortField = do
    config <- getConfig
    tryRepeatedlyTo $ byCss config.configureMount.portField >>= findExact

  getHostField :: SlamFeature Element
  getHostField = do
    config <- getConfig
    tryRepeatedlyTo $ byCss config.configureMount.hostField >>= findExact

  getMountDatabaseButton :: SlamFeature Element
  getMountDatabaseButton = do
    config <- getConfig
    tryToFind $ byAriaLabel config.toolbar.mountDatabase

goodMountDatabase :: SlamFeature Unit
goodMountDatabase = do
  sectionMsg "MOUNT TEST DATABASE"
  mountDatabaseWithMountConfig =<< mountConfigFromConfig
  tryRepeatedlyTo expectMountShown

  where
  expectMountShown :: SlamFeature Unit
  expectMountShown = do
    config <- getConfig
    void $ findItem config.mount.name

badMountDatabase :: SlamFeature Unit
badMountDatabase = do
  sectionMsg "BAD MOUNT TEST DATABASE"
  config <- getConfig
  mountDatabaseWithMountConfig =<< badMountConfig
  warningBox <- getElementByCss config.configureMount.warningBox "no warning box"
  -- wait for any old validation messages to disappear
  wait (checker $ not <$> isDisplayed warningBox) config.selenium.waitTime
  -- wait for the server error to appear
  wait (checker $ isDisplayed warningBox) 8000

  tryRepeatedlyTo
    $ getElementByCss config.configureMount.cancelButton "no cancel button"
    >>= sequence <<< leftClick
  waitModalDismissed
  where
    badMountConfig :: SlamFeature MountConfigR
    badMountConfig = do
      mountConfig <- mountConfigFromConfig
      pure $ mountConfig { port = mountConfig.port - 1 }

unmountDatabase :: SlamFeature Unit
unmountDatabase = do
  sectionMsg "UNMOUNT TEST DATABASE"
  home
  config <- getConfig
  click' =<< findItem config.mount.name
  click' =<< itemGetDeleteIcon =<< findItem config.mount.name
  loseItem config.mount.name
  successMsg "successfully unmounted"

checkMountedDatabase :: SlamFeature Unit
checkMountedDatabase = do
  sectionMsg "CHECK TEST DATABASE IS MOUNTED"
  enterMount
  void $ findTestDb
  successMsg "test database found"

checkConfigureMount :: SlamFeature Unit
checkConfigureMount = do
  sectionMsg "CHECK CONFIGURE MOUNT DIALOG"
  enterMount

  button <- getConfigureMountButton
  successMsg "got configure-mount button"
  sequence $ leftClick button

  config <- getConfig
  waitModalShown
  successMsg "configure-mount dialog shown"

  -- make sure a no-op edit doesn't result in a validation error
  usernameField <- getElementByCss config.configureMount.usernameField "no usernameField field"

  modifierKey <- getModifierKey
  sequence do
    leftClick usernameField
    sendBackspaces 100
    keys "hello"
    undo modifierKey

  getElementByCss config.configureMount.saveButton "no save button"
    >>= isEnabled
    >>= assertBoolean "save button should be enabled"

  where
  getConfigureMountButton :: SlamFeature Element
  getConfigureMountButton = do
    config <- getConfig
    tryToFind $ byAriaLabel config.toolbar.configureMount

enterMount :: SlamFeature Unit
enterMount = do
  home
  url <- getCurrentUrl
  config <- getConfig
  click' =<< findOpenItem config.mount.name
  wait (awaitUrlChanged url) config.selenium.waitTime
  fileComponentLoaded

moveDeleteDatabase :: SlamFeature Unit
moveDeleteDatabase = do
  config <- getConfig
  moveDelete "database" home config.mount.name config.mount.otherName

testMountDatabase :: SlamFeature Unit
testMountDatabase = do
  home
  -- badMountDatabase
  -- goodMountDatabase
  -- moveDeleteDatabase
  -- goodMountDatabase
  -- unmountDatabase
  goodMountDatabase
  checkMountedDatabase
  checkConfigureMount
