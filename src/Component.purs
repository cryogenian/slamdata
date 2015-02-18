module Component (
  Receiver(..),
  Service(..),
  Widget(..),
  WidgetSpec(..),
  UpdateFn(..),
  RenderFn(..),
  Initial(..),
  define,
  toVoid
  ) where

import DOM
import Signal
import Signal.Channel
import Signal.Effectful
import VirtualDOM
import VirtualDOM.VTree
import Control.Monad.Eff
import Utils
import View.Shortcuts

{-
TODO : Think about Widgets and GlobalComponents and how to
make functions work with them both. Is it better to use
row polymorphism or typeclasses
-}


type Receiver action e = action -> Eff e Unit
type UpdateFn action state eff = action -> state -> Eff eff state
type RenderFn action state eff = Receiver action eff -> state -> Eff eff VTree
type Initial action state = {action :: action, state :: state}

type Folder a = {
  current :: VTree,
  previous :: VTree,
  state :: a
  }

type Service action state eff = {
  signal :: Signal state,
  send :: Receiver action eff
  }

type Widget action state eff = {
  signal :: Signal state,
  send :: Receiver action eff,
  insert :: Node -> Eff eff Unit
  }

type WidgetSpec action state eff = {
  render ::  RenderFn action state eff,
  updateState :: UpdateFn action state eff,
  initial :: Initial action state,
  hook :: Receiver action eff -> Eff eff Unit
  }


mkFolder :: forall state. state -> Folder state
mkFolder state = {
  current: emptyVTree,
  previous: emptyVTree,
  state: state
  }

toVoid :: forall a e. Receiver a e
toVoid a = return unit

define :: forall state action e.
             WidgetSpec action state (chan::Chan, dom::DOM|e) ->
             Eff (chan::Chan, dom::DOM|e)   
                 (Widget action state (chan::Chan, dom::DOM|e))
define spec@{render: render,
             initial: initial,
             updateState: updateState,
             hook: hook} = do
  chan <- channel initial.action

  let folder = mkFolder initial.state
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


  let component = {
        signal: allSignal ~> _.state,
        send: send chan,
        insert: \node -> do
          start allSignal node
          hook component.send
        }
  return $ component
  
