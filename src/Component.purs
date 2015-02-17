module Component where

import DOM
import Signal
import Signal.Channel
import Signal.Effectful
import VirtualDOM
import VirtualDOM.VTree
import Control.Monad.Eff
import Utils
import View.Shortcuts

type Folder a = {
  current :: VTree,
  previous :: VTree,
  state :: a
  }

mkFolder :: forall state. state -> Folder state
mkFolder state = {
  current: emptyVTree,
  previous: emptyVTree,
  state: state
  }


type Receiver action e = action -> Eff (chan::Chan|e) Unit
toVoid :: forall a e. Receiver a e
toVoid a = return unit

type Component action state = {
  signal :: Signal (Folder state),
  channel :: Channel action,
  vt :: VTree
  }

type FoldFn action state eff = action -> state -> Eff eff state
type RenderFn action state eff = Receiver action eff -> state -> Eff eff VTree

start :: forall a b. Component a b -> Node -> Eff _ Unit
start component node = do
  let t = createElement component.vt
  append node t
  runSignal $ component.signal ~> \all -> do
    let d = diff all.previous all.current
    void $ patch d t




foldStateToFoldAll :: forall action state.
                      FoldFn action state _ -> 
                      RenderFn action state _ ->
                      Receiver action _  ->
                      (action ->
                       Folder state ->
                       Eff _ (Folder state))
foldStateToFoldAll foldState view receiver =
  \action {state: state, current: current, previous: _} -> do
    new <- foldState action state
    newVt <- view receiver new
    return {state: new, previous: current, current: newVt}




