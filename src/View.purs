module View where

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

foldAll :: Receiver Action _ -> Action -> Folder State -> Eff _ (Folder State)
foldAll receiver action {state: state, current: current, previous: previous} = do
  new <- foldState action state
  newVt <- view receiver new
  return {state: new, previous: current, current: newVt}


spec :: ComponentSpec State Action _ 
spec = {
  initialState: initialState,
  initSignal: Init,
  render: view,
  updateState: foldState
  }

{-
construct :: Eff _ (Component Action State)
construct = do
  chan <- channel Init
  vt <- view (send chan) initialState
  let folder = mkFolder initialState
  signal <- foldpE (foldAll (send chan)) folder (subscribe chan)

  -- This stuff definitely works not how it must.
  -- There must be something like "init" function
  -- or postConstruct, that will be hooked after construction
  -- of component.
  hashComponent <- Hash.construct
  runSignal $ hashComponent.signal ~> \hash -> do
    send chan (NavbarAction <<< Navbar.SearchAction <<< Search.HashChanged $ hash.state)

  return $ {
    signal: signal,
    channel: chan,
    vt: vt
    }
  
         -}
      
