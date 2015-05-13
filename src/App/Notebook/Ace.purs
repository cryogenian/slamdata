module App.Notebook.Ace (acePostRender, ref, AceKnot()) where

import Input.Notebook (CellResultContent(..), Input(..))

import Control.Bind ((>=>))
import Control.Monad (when)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Ref (Ref(), RefVal(), readRef, newRef, modifyRef)
import Data.Foldable (for_)

import Data.Tuple
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.Date (Now(), now)

import qualified Data.Map as M

import DOM
import Data.DOM.Simple.Element
import Data.DOM.Simple.Types

import Halogen

import Ace
import Ace.EditSession (getValue, setMode)
import Ace.Selection (getRange)
import Ace.Types (EditSession(), EAce(), Editor(), TextMode())
import qualified Ace.Editor as Editor

import Model.Notebook.Cell (CellId(), string2cellId)

import Optic.Core
import Optic.Refractor.Lens 

type AceKnot = Tuple CellId (M.Map CellId EditSession)

foreign import markdownMode """
  var markdownMode = "/ace/mode/markdown";
""" :: TextMode
foreign import plainTextMode """
  var plainTextMode = "ace/mode/plain_text";
""" :: TextMode

modeByCellTag :: String -> TextMode
modeByCellTag tag = case tag of
  "markdown" -> markdownMode
  _ -> plainTextMode


dataCellId :: String
dataCellId = "data-cell-id"

dataCellType :: String
dataCellType = "data-cell-type"

initialize :: forall eff. RefVal AceKnot -> HTMLElement ->
              Driver Input (ace :: EAce | eff) -> 
              Eff (HalogenEffects (ace :: EAce | eff)) Unit
initialize m b d = do
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
    modifyRef m $ \x -> x # _2 %~ M.insert cid session
    Editor.onFocus editor do
      modifyRef m $ \x -> x # _1 .~ cid 
      d $ SetActiveCell cid

  reinit :: Editor -> EditSession -> Eff _ Unit
  reinit editor session = do
    Editor.setSession session editor


handleInput :: forall eff. RefVal AceKnot -> Input ->
               Driver Input (now :: Now, ace :: EAce | eff) -> Eff (HalogenEffects (now :: Now, ace :: EAce | eff)) Unit
handleInput m (RunCell cellId _) d = do
  Tuple _ m' <- readRef m
  now' <- now
  maybe (return unit) (getValue >=> d <<< CellResult cellId now' <<< Right <<< AceContent) $
    M.lookup cellId m'
handleInput m (TrashCell cellId) _ = do
  modifyRef m $ (\x -> x # _2 %~ M.delete cellId)
handleInput _ _ _ = return unit

acePostRender :: forall eff. RefVal AceKnot -> Input ->
                 HTMLElement -> Driver Input (now :: Now, ace :: EAce | eff) -> Eff (HalogenEffects (now :: Now, ace :: EAce | eff)) Unit
acePostRender m i b d = do
  initialize m b d
  handleInput m i d

ref :: forall e.  Eff (ref :: Ref | e) (RefVal AceKnot) 
ref = newRef (Tuple 0 M.empty)
