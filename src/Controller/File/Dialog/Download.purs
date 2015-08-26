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

module Controller.File.Dialog.Download where

import Prelude
import Control.Plus (empty)
import Control.Monad.Eff.Class (liftEff)
import Controller.File.Common (Event(), toInput)
import Data.Array (findIndex)
import Data.DOM.Simple.Types (HTMLElement())
import Data.DOM.Simple.Element (getAttribute)
import Data.Either (Either(..), either, isLeft)
import Data.Maybe (Maybe(..), maybe, isJust)
import Data.String (indexOf)
import Input.File (FileInput(..))
import Model.File (State(), _dialog)
import Model.File.Dialog (_DownloadDialog)
import Model.File.Dialog.Download
import Model.Path (parseAnyPath)
import Model.Resource (Resource(..), getPath, resourceName)
import Optic.Core 
import Optic.Refractor.Prism (_Just)
import Utils (newTab)

import qualified Data.Either.Unsafe as U

handleSourceInput :: forall e. String -> Event e
handleSourceInput s = toInput' $ _source .~ maybe (Left s) (Right <<< either File Directory) (parseAnyPath s)

handleSourceClicked :: forall e. Resource -> Event e
handleSourceClicked r = toInput' $ (_showSourcesList .~ false)
                                .. (_targetName .~ Right (resourceName r))
                                .. (_source .~ Right r)

handleToggleList :: forall e. Event e
handleToggleList = toInput' $ _showSourcesList %~ not

handleTargetNameChange :: forall e. String -> Event e
handleTargetNameChange s =
  toInput' $ _targetName .~ (if isJust $ indexOf "/" s then Left else Right) s

handleCompressToggle :: forall e. Event e
handleCompressToggle = toInput' $ _compress %~ not

handleChangeOutput :: forall e. OutputType -> Event e
handleChangeOutput ty = toInput' $ _options %~ case ty of
  CSV -> Left <<< either id (const initialCSVOptions)
  JSON -> Right <<< either (const initialJSONOptions) id

handleOptionChange :: forall e. (Either CSVOptions JSONOptions -> Either CSVOptions JSONOptions) -> Event e
handleOptionChange f = toInput' $ _options %~ f

handleDownloadClick :: forall e. HTMLElement -> Event e
handleDownloadClick el = do
  liftEff $ getAttribute "href" el >>= newTab
  empty

toInput' :: forall e. (DownloadDialogRec -> DownloadDialogRec) -> Event e
toInput' f = toInput $ WithState $ _dialog .. _Just .. _DownloadDialog %~ validate <<< f

validate :: DownloadDialogRec -> DownloadDialogRec
validate rec
  | isLeft (rec ^. _source) = rec # _error ?~ "Please enter a valid source path to download"
  | not $ checkExists (U.fromRight $ rec ^. _source) (rec ^. _sources) = rec # _error ?~ "The source resource does not exist"
  | isLeft (rec ^. _targetName) = rec # _error ?~ "Please enter a valid target filename"
  | otherwise = rec # _error .~ Nothing

checkExists :: Resource -> Array Resource -> Boolean
checkExists r rs =
  let path = getPath r
  in isJust $ findIndex (\r' -> getPath r' == path) rs 
