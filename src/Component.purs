-- | **action** - type of messages to components
-- | it modifies **state** in function UpdateFn

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
import Data.DOM.Simple.Types
import Signal
import Signal.Channel
import Signal.Effectful
import VirtualDOM
import VirtualDOM.VTree
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.ST
import Utils
import View.Shortcuts

{-
TODO : Think about Widgets and GlobalComponents and how to
make functions work with them both. Is it better to use
row polymorphism or typeclasses
-}



-- | This is type of message receiver in component
-- | It takes an **action** and do something with it
type Receiver action e = action -> Eff e Unit

-- | this function fold **state** with **action**
type UpdateFn action state eff = action -> state -> Eff eff state
-- | this function modifies virtual tree 
type RenderFn action state eff = Receiver action eff -> state -> Eff eff VTree
-- | This is just for reducing number of fields in spec 
type Initial action state = {action :: action, state :: state}

-- | This type is used in **start** function it holds
-- | vtrees to calculate diffs and patch
type Folder a = {
  current :: VTree,
  previous :: VTree,
  state :: a
  }

-- | WIP : This is component that has no render function
-- | I think it must be global in some cases.
-- | I suggest to use it for FileUploader, ApiCalls and Router
type Service action state eff = {
  signal :: Signal state,
  send :: Receiver action eff
  }

-- | This is basic component that can be rendered to DOM
type Widget action state eff = {
  -- | Output signal, it populate **state** of component
  -- | on every update
  signal :: Signal state,
  -- | Function that take **action** and put it to FoldFn
  send :: Receiver action eff,
  -- | Entry point of component (something like **renderTo** in ReactJS)
  insert :: HTMLElement -> Eff eff Unit
  }

-- | Spec of component
type WidgetSpec action state eff = {
  render ::  RenderFn action state eff,
  updateState :: UpdateFn action state eff,
  initial :: Initial action state,
  -- | This function will be called after inserting to DOM
  hook :: Receiver action eff -> Eff eff Unit
  }

-- | internal : makes Folder from state
mkFolder :: forall state. state -> Folder state
mkFolder state = {
  current: emptyVTree,
  previous: emptyVTree,
  state: state
  }

-- | Shortcut to void receiver
toVoid :: forall a e. Receiver a e
toVoid a = return unit


-- | Takes **WidgetSpec** returns **Widget**
define :: forall state action e.
             WidgetSpec action state (chan::Chan, dom::DOM|e) ->
             Eff (chan::Chan, dom::DOM|e)   
                 (Widget action state (chan::Chan, dom::DOM|e))
define spec@{render: render,
             initial: initial,
             updateState: updateState,
             hook: hook} = do
  -- Initializing of input channel
  chan <- channel initial.action

  -- Make accumulator
  let folder = mkFolder initial.state
  -- | signal folding function. It takes accumulator of Folder State
  -- | used to make output signal
  let foldAll receiver action {state: state,
                               current: current,
                               previous: previous} = do
        -- make new state
        new <- updateState action state
        -- make new vtree
        newVt <- render receiver new
        -- update state
        return {state: new, previous: current, current: newVt}

  -- make signal that oupuput Folder State
  allSignal <- foldpE (foldAll (send chan)) folder (subscribe chan)

  -- | This is exactly function that update DOM
  let start folderSignal node = do
        -- make element
        let t = createElement emptyVTree
        -- add it to DOM
        append node (convertToElement t)
        -- on every folder update
        runSignal $ folderSignal ~> \all ->
          -- update DOM
          void $ flip patch t $ diff all.previous all.current


  -- return component
  let component = {
        -- | Filter signal to only state
        signal: allSignal ~> _.state,
        -- | Provide shortcuts to send action
        send: send chan,
        -- | Provide entry point that
        insert: \node -> do
          -- | start render
          start allSignal node
          -- | call hooks
          hook component.send
        }
  -- It's important to call render before hooks
  -- in other case it will not render 
  return $ component
  


