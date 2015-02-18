module View (State(..), Action(..), spec) where

import DOM
import Control.Monad.Eff
import Signal
import Signal.Effectful
import Signal.Channel
import VirtualDOM
import VirtualDOM.VTree

import Utils
import Component
import View.Shortcuts 
import qualified View.Navbar as Navbar
import qualified View.Search as Search
import qualified View.List as List
import qualified View.Toolbar as Toolbar
import qualified Hash as Hash

type State = {
  navbar :: Navbar.State,
  list :: List.State,
  toolbar :: Toolbar.State
  }

initialState :: State
initialState = {
  navbar: Navbar.initialState,
  list: List.initialState,
  toolbar: Toolbar.initialState
  }

data Action = Init
            | ListAction List.Action
            | NavbarAction Navbar.Action
            | ToolbarAction Toolbar.Action

view :: Receiver Action _ -> State -> Eff _ VTree
view send st = do
  navbar <- Navbar.view (send <<< NavbarAction) st.navbar
  list <- List.view (send <<< ListAction) st.list
  toolbar <- Toolbar.view (send <<< ToolbarAction) st.toolbar

  return $ div {} [
    navbar,
    div {"className": "container"} [
      toolbar,
      list
      ]
    ]

foldState :: Action -> State -> Eff _ State
foldState action state =
  case action of
    Init -> return initialState
    ListAction action ->
      state{list = _} <$> List.foldState action state.list
    NavbarAction action ->
      state{navbar = _} <$> Navbar.foldState action state.navbar
    ToolbarAction action ->
      state{toolbar = _} <$> Toolbar.foldState action state.toolbar

initial :: Initial Action State
initial = 
  {action: Init,
   state: initialState}
          

hook :: forall e. Receiver Action (chan::Chan|e) -> Eff (chan::Chan|e) Unit
hook receiver = do
  Navbar.hook (receiver <<< NavbarAction)

spec :: WidgetSpec Action State _ 
spec = {
  render: view,
  initial: initial,
  updateState: foldState,
  hook: hook
  }
