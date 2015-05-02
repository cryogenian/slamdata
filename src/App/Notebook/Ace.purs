module App.Notebook.Ace where

import Model.Notebook (Input(..))

import Data.Void
import Data.Maybe
import Data.Tuple
import Data.Either
import Data.Function
import Data.Profunctor (lmap)

import Control.Functor (($>))
import Control.Bind
import Control.Monad.Eff

import DOM

import Data.DOM.Simple.Document
import Data.DOM.Simple.Element
import Data.DOM.Simple.Types
import Data.DOM.Simple.Window

import Debug.Trace

import Halogen
import Halogen.Signal
import Halogen.Component
import Halogen.Internal.VirtualDOM (Widget())

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A

import qualified Halogen.Themes.Bootstrap3 as B

import Ace
import Ace.Types (EAce(), Editor())

import qualified Ace.Editor as Editor

foreign import createEditorNode
  "function createEditorNode() {\
  \  return document.createElement('div');\
  \}" :: forall eff. Eff (dom :: DOM | eff) HTMLElement

-- | The application state, which stores the string most recently copied to the clipboard.
data State = State (Maybe String)

ui :: forall m eff. (Applicative m) => Component (Widget (HalogenEffects (ace :: EAce | eff)) Input) m Input Input
ui = runComponent (component' <<< (render <$>)) aceEditor
  where
  render :: forall a. H.HTML _ a -> H.HTML _ a
  render c  = H.div [ A.class_ (A.className "ace-container") ] [ c ]

-- | The Ace editor is represented as a `Component`, created using `Component.widget`.
aceEditor :: forall m eff. (Functor m) => Component (Widget (HalogenEffects (ace :: EAce | eff)) Input) m Input Input
aceEditor = widget { name: "AceEditor", id: "editor1", init: init, update: update, destroy: destroy }
  where
  init :: forall eff. (Input -> Eff (ace :: EAce, dom :: DOM | eff) Unit) -> Eff (ace :: EAce, dom :: DOM | eff) { state :: Editor, node :: HTMLElement }
  init driver = do
    node <- createEditorNode
    editor <- Ace.editNode node ace
    Editor.setTheme "ace/theme/monokai" editor
    Editor.onCopy editor (driver <<< AceTextCopied)
    return { state: editor, node: node }

  update :: forall eff. Input  -> Editor -> HTMLElement -> Eff (ace :: EAce, dom :: DOM | eff) (Maybe HTMLElement)
  update (RunCell _) editor _ = Editor.setValue "" Nothing editor $> Nothing
  update _ _ _ = return Nothing

  destroy :: forall eff. Editor -> HTMLElement -> Eff (ace :: EAce, dom :: DOM | eff) Unit
  destroy editor _ = Editor.destroy editor
