module App.Notebook.Ace (acePostRender, ref, AceKnot()) where

import Input.Notebook (CellResultContent(..), Input(..))

import Control.Bind ((>=>))
import Control.Monad (when)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Ref (Ref(), RefVal(), readRef, newRef, modifyRef)

import Data.Date (Now(), now)
import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.Tuple

import qualified Data.Map as M

import DOM
import Data.DOM.Simple.Element
import Data.DOM.Simple.Types

import Halogen

import Ace
import qualified Ace.Config as AceConfig
import Ace.EditSession (getValue, setMode)
import Ace.Selection (getRange)
import Ace.Types (EditSession(), ACE(), Editor(), TextMode(..))
import qualified Ace.Editor as Editor

import Model.Notebook
import Model.Notebook.Cell (CellId(), string2cellId)

import Optic.Core
import Optic.Refractor.Lens

type AceKnot = Tuple CellId (M.Map CellId EditSession)

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

initialize :: forall eff. RefVal AceKnot -> HTMLElement ->
              Driver Input (ace :: ACE | eff) ->
              Eff (HalogenEffects (ace :: ACE | eff)) Unit
initialize m b d = do
  AceConfig.set AceConfig.basePath (Config.baseUrl ++ "js/ace")
  els <- getElementsByClassName "ace-container" b
  Tuple _ mr <- readRef m
  for_ els \el -> do
    cellId <- string2cellId <$> getAttribute dataCellId el
    flip (either (const $ pure unit)) cellId \cid -> do
      mode <- modeByCellTag <$> getAttribute dataCellType el
      editor <- Ace.editNode el ace
      Editor.setTheme "ace/theme/chrome" editor
      maybe (initialize' mode editor cid) (reinit editor) $ M.lookup cid mr
  where
  initialize' :: _ -> _ -> CellId -> Eff _ Unit
  initialize' mode editor cid = do
    session <- createEditSession "" mode ace
    Editor.focus editor
    modifyRef m (_2 %~ M.insert cid session)
    Editor.onFocus editor do
      modifyRef m (_1 .~ cid)
      d $ WithState (_activeCellId .~ cid)


  reinit :: Editor -> EditSession -> Eff _ Unit
  reinit editor session = do
    Editor.setSession session editor


handleInput :: forall eff. RefVal AceKnot
                        -> Input
                        -> Driver Input (now :: Now, ace :: ACE | eff)
                        -> Eff (HalogenEffects (now :: Now, ace :: ACE | eff)) Unit
handleInput m (RunCell cellId _) d = do
  Tuple _ m' <- readRef m
  now' <- now
  maybe (return unit) (getValue >=> d <<< CellResult cellId now' <<< Right <<< AceContent) $
    M.lookup cellId m'
handleInput m (TrashCell cellId) _ = do
  modifyRef m (_2 %~ M.delete cellId)
handleInput _ _ _ = return unit

acePostRender :: forall eff. RefVal AceKnot -> Input ->
                 HTMLElement -> Driver Input (now :: Now, ace :: ACE | eff) -> Eff (HalogenEffects (now :: Now, ace :: ACE | eff)) Unit
acePostRender m i b d = do
  initialize m b d
  handleInput m i d

ref :: forall e.  Eff (ref :: Ref | e) (RefVal AceKnot)
ref = newRef (Tuple 0 M.empty)
