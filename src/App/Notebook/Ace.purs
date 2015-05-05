module App.Notebook.Ace where

import Model.Notebook (Input(..))

import Data.Maybe
import Data.Tuple
import Data.Either
import Data.Function
import Data.Foldable (for_)
import Data.Profunctor (lmap)

import Control.Functor (($>))
import Control.Plus (empty)
import Control.Bind
import Control.Monad.Eff
import Control.Monad.Eff.Class

import DOM

import Data.DOM.Simple.Document
import Data.DOM.Simple.Element
import Data.DOM.Simple.Types
import Data.DOM.Simple.Window

import Debug.Trace

import Halogen
import Halogen.Signal
import Halogen.Component

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A
import qualified Halogen.HTML.Events.Monad as E

import qualified Halogen.Themes.Bootstrap3 as B

import Ace
import Ace.Types (EAce(), Editor())

import qualified Ace.Editor as Editor

foreign import createEditorNode
  "function createEditorNode() {\
  \  return document.createElement('div');\
  \}" :: forall eff. Eff (dom :: DOM | eff) HTMLElement

data State = State String

ui :: forall eff. String -> Component (E.Event (dom :: DOM | eff)) Input Input
ui s = render <$> stateful (State "") const
  where
  render :: State -> H.HTML (E.Event (dom :: DOM | eff) Input)
  render (State c) = H.div [ A.initializer (initializer s)
                           , A.classes [A.className "ace-container", A.className ("ace" <> s)]
                           ] [ ]

-- TOOD: Get events out.
initializer :: forall eff. String -> E.Event (dom :: DOM | eff) Input
initializer s = do
  liftEff $ do
    doc <- document globalWindow
    b <- body doc
    els <- querySelector (".ace-" <> s) b
    for_ els \el -> do
      -- Setup the Ace editor
      editor <- Ace.editNode el ace
      session <- Editor.getSession editor
      Editor.setTheme "ace/theme/monokai" editor
  empty

-- TOOD: Add a finalizer.
