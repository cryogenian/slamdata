module App.Notebook.Ace (acePostRender, ref) where

import Input.Notebook (Input(..))

import Control.Bind ((>=>))
import Control.Monad (when)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Ref (Ref(), RefVal(), readRef, newRef, writeRef)
import Data.Foldable (for_)

import Data.Either (either)
import Data.Maybe (isNothing, maybe)
import qualified Data.Map as M

import DOM
import Data.DOM.Simple.Element
import Data.DOM.Simple.Types

import Halogen

import Ace
import Ace.EditSession (getValue, setMode)
import Ace.Types (EditSession(), EAce(), Editor())
import Model.Notebook (CellId(), string2cellId)

import qualified Ace.Editor as Editor

modeByCellTag :: String -> String
modeByCellTag tag = case tag of
  "markdown" -> "ace/mode/markdown"
  _ -> "ace/mode/plain_text"


initialize :: forall eff. RefVal (M.Map CellId EditSession) -> HTMLElement ->
              Driver Input (ace :: EAce | eff) -> 
              Eff (HalogenEffects (ace :: EAce | eff)) Unit
initialize m b d = do
  els <- getElementsByClassName "ace-container" b
  m' <- readRef m
  for_ els \el -> do
    cellId <- string2cellId <$> getAttribute "data-cell-id" el
    flip (either (const $ pure unit)) cellId \cid -> do
      when (isNothing $ M.lookup cid m') $ do
        mode <- modeByCellTag <$> getAttribute "data-cell-type" el
        editor <- Ace.editNode el ace
        session <- Editor.getSession editor
        setMode mode session
        Editor.setTheme "ace/theme/github" editor

        writeRef m $ M.insert cid session m'
        Editor.onFocus editor (d $ SetActiveCell cid)

handleInput :: forall eff. RefVal (M.Map CellId EditSession) -> Input ->
               Driver Input (ace :: EAce | eff) -> Eff (HalogenEffects (ace :: EAce | eff)) Unit
handleInput m (RunCell cellId) d = do
  m' <- readRef m
  maybe (return unit) (getValue >=> d <<< AceContent cellId) $
    M.lookup cellId m'
handleInput _ _ _ = return unit

acePostRender :: forall eff. RefVal (M.Map CellId EditSession) -> Input ->
                 HTMLElement -> Driver Input (ace :: EAce | eff) -> Eff (HalogenEffects (ace :: EAce | eff)) Unit
acePostRender m i b d = do
  initialize m b d
  handleInput m i d

ref :: forall e.  Eff (ref :: Ref | e) (RefVal (M.Map CellId EditSession)) 
ref = newRef M.empty
