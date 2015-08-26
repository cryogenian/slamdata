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

module Driver.Notebook.Ace (acePostRender, AceSessions()) where

import Prelude
import Ace
import Ace.EditSession (getValue, setMode)
import Ace.Selection (getRange)
import Ace.Types (EditSession(), ACE(), Editor(), TextMode(..))
import Control.Bind ((>=>))
import Control.Monad (when)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Ref (Ref(), readRef, modifyRef)
import Data.Date (Now(), now)
import Data.DOM.Simple.Element
import Data.DOM.Simple.Types
import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isNothing, maybe, fromMaybe)
import Data.Tuple
import DOM
import Global (infinity)
import Halogen
import Input.Notebook (CellResultContent(..), Input(..), cellContent)
import Model.Notebook
import Model.Notebook.Cell (CellContent(..), CellId(), _AceContent, string2cellId, _cellId, _content)
import Model.Notebook.Domain
import Optic.Core
import Optic.Extended ((^?))
import Optic.Refractor.Lens

import qualified Ace.Config as AceConfig
import qualified Ace.Editor as Editor
import qualified Data.Map as M

type AceSessions = M.Map CellId EditSession

markdownMode :: TextMode
markdownMode = TextMode "ace/mode/markdown"

sqlMode :: TextMode
sqlMode = TextMode "ace/mode/sql"

plainTextMode :: TextMode
plainTextMode = TextMode "ace/mode/plain_text"

modeByCellTag :: String -> TextMode
modeByCellTag tag = case tag of
  "markdown" -> markdownMode
  "query" -> sqlMode
  _ -> plainTextMode

dataCellId :: String
dataCellId = "data-cell-id"

dataCellType :: String
dataCellType = "data-cell-type"

-- TODO: Put this in purescript-ace and fix parametricity.
foreign import aceSetOption :: forall a eff. String -> a -> Editor ->
                               Eff (ace :: ACE | eff) Unit

initialize :: forall eff. Ref State
                       -> Ref AceSessions
                       -> HTMLElement
                       -> Driver Input (ace :: ACE | eff)
                       -> Eff (HalogenEffects (ace :: ACE | eff)) Unit
initialize s m b d = do
  AceConfig.set AceConfig.basePath (Config.baseUrl ++ "js/ace")
  els <- getElementsByClassName "ace-container" b
  mr <- readRef m
  state <- readRef s
  for_ els \el -> do
    cellId <- string2cellId <$> getAttribute dataCellId el
    flip (either (const $ pure unit)) cellId \cid -> do
      mode <- modeByCellTag <$> getAttribute dataCellType el
      editor <- Ace.editNode el ace
      aceSetOption "minLines" 4 editor
      aceSetOption "maxLines" infinity editor
      aceSetOption "autoScrollEditorIntoView" true editor

      Editor.setTheme "ace/theme/chrome" editor
      maybe (initialize' mode editor state cid) (reinit editor) $ M.lookup cid mr
  where
  initialize' :: _ -> _ -> State -> CellId -> Eff _ Unit
  initialize' mode editor state cid = do
    let cell = state ^? _notebook .. cellById cid .. _content .. _AceContent
        value = fromMaybe "" cell
    session <- createEditSession value mode ace
    Editor.focus editor
    modifyRef m (M.insert cid session)
    Editor.onFocus editor do
      d $ WithState (_notebook .. _activeCellId .~ cid)

  reinit :: Editor -> EditSession -> Eff _ Unit
  reinit editor session = do
    Editor.setSession session editor

handleInput :: forall eff. Ref AceSessions
                        -> Input
                        -> Driver Input (now :: Now, ace :: ACE | eff)
                        -> Eff (HalogenEffects (now :: Now, ace :: ACE | eff)) Unit
handleInput m (RequestCellContent cell) d = do
  m' <- readRef m
  now' <- now
  let cellId = cell ^. _cellId
  let updateCell c = cellContent (Right $ AceContent c) cell
  maybe reemit (getValue >=> d <<< ReceiveCellContent <<< updateCell) $
    M.lookup cellId m'
  where
  reemit =
    case cell ^._content of
      Explore _ -> pure unit
      Search _ -> pure unit
      _ -> d $ ReceiveCellContent cell

handleInput m (TrashCell cellId) _ = do
  modifyRef m (M.delete cellId)
handleInput _ _ _ = return unit

acePostRender :: forall eff. Ref State
                          -> Ref AceSessions
                          -> Input
                          -> HTMLElement
                          -> Driver Input (now :: Now, ace :: ACE | eff)
                          -> Eff (HalogenEffects (now :: Now, ace :: ACE | eff)) Unit
acePostRender s m i b d = do
  initialize s m b d
  handleInput m i d


