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

type Component action state eff = {
  signal :: Signal state,
  channel :: Channel action,
  insert :: Node -> Eff eff Unit
  }

type FoldFn action state eff = action -> state -> Eff eff state
type RenderFn action state eff = Receiver action eff -> state -> Eff eff VTree

type ComponentSpec state action e1 = {
  render ::  Receiver action e1 -> state -> Eff e1 VTree,
  initialState :: state,
  initSignal :: action,
  updateState :: action -> state -> Eff e1 state
  }


--define :: forall state action e.
--             ComponentSpec state action (chan::Chan, dom::DOM|e) ->
--             Eff (chan::Chan, dom::DOM|e)   
--                 (Component action state (chan::Chan, dom::DOM|e))

define spec@{render: render,
                initialState: initialState,
                updateState: updateState,
                initSignal: initSignal} = do
  chan <- channel initSignal
  let folder = mkFolder initialState
  let foldAll receiver action {state: state,
                               current: current,
                               previous: previous} = do
        new <- updateState action state
        newVt <- render receiver new
        return {state: new, previous: current, current: newVt}

  allSignal <- foldpE (foldAll (send chan)) folder (subscribe chan)

  let start folderSignal node = do
        let t = createElement emptyVTree
        append node t
        runSignal $ folderSignal ~> \all ->
          void $ flip patch t $ diff all.previous all.current


  
  return {
    signal: allSignal ~> _.state,
    channel: chan,
    insert: start allSignal
    }
  
