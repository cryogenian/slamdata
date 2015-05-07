module App.Notebook.Ace (acePostRender) where

import Model.Notebook (Input(..))

import Control.Bind ((>=>))
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Ref (Ref(), RefVal(), modifyRef, readRef)
import Data.Foldable (for_)

import Data.Maybe (maybe)
import qualified Data.StrMap as M

import DOM
import Data.DOM.Simple.Element
import Data.DOM.Simple.Types

import Halogen

import Ace
import Ace.EditSession (getValue)
import Ace.Types (EditSession(), EAce(), Editor())

import qualified Ace.Editor as Editor

initialize :: forall eff. RefVal (M.StrMap EditSession) -> HTMLElement -> Eff (HalogenEffects (ace :: EAce | eff)) Unit
initialize m b = do
  els <- getElementsByClassName "ace-container" b
  for_ els \el -> do
    -- Setup the Ace editor
    editor <- Ace.editNode el ace
    session <- Editor.getSession editor
    Editor.setTheme "ace/theme/monokai" editor

    cellId <- getAttribute "data-cell-id" el
    modifyRef m $ M.insert cellId session

handleInput :: forall eff. RefVal (M.StrMap EditSession) -> Input -> Driver Input (ace :: EAce | eff) -> Eff (HalogenEffects (ace :: EAce | eff)) Unit
handleInput m (RunCell cellId) d = do
  m' <- readRef m
  maybe (return unit) (getValue >=> d <<< AceContent cellId) $ M.lookup cellId m'
handleInput _ _ _ = return unit

acePostRender :: forall eff. RefVal (M.StrMap EditSession) -> Input -> HTMLElement -> Driver Input (ace :: EAce | eff) -> Eff (HalogenEffects (ace :: EAce | eff)) Unit
acePostRender m i b d = do
  initialize m b
  handleInput m i d
