-- | Shortcuts for VirtualDOM events
-- | They are not well typed, so it's possible
-- | to put in "onclick" everything
-- | it works when we put  Eff _ Unit there or
-- | Callback defined here
module VirtualDOM.Events where

import Control.Monad.Eff
import Data.Function
import DOM (DOM(), Node())
import Data.DOM.Simple.Types (HTMLElement())
import VirtualDOM.VTree (VTree(), VHook(), vhook)
import Control.Reactive.Event (Event())


foreign import data Callback :: *

-- | Will be changed in favor of HTMLElement, I suppose
type Handler e = Node -> Event -> Eff e Unit 

foreign import mkCallback """
function mkCallback(handler) {
  return function(event) {
    handler(event.target)(event)();
  };
}
""" :: forall e.  Handler e -> Callback

-- | emptyHandler
foreign import returnFalse """
function returnFalse() {
  return false;
}
""" :: Callback

-- copied from https://github.com/fluffynukeit/purescript-puzzler/blob/master/src/View.purs

type HookFn = Fn2 Node String (Eff (dom::DOM) Unit)

type Listener = { event::String, callback::Callback}

listener :: forall e. String -> (Event -> Eff e Unit) -> Listener
listener s a = {event:s, callback: mkCb a}
foreign import mkCb """
  function mkCb (act) {
    return function (event) {
      act(event)();
    };
  } """ :: forall e. (Event -> Eff e Unit) -> Callback

foreign import listen  """
  function listen (l) {
    return function (node, prop) {
      node.addEventListener(l.event, l.callback);
    };
  };""" :: Listener -> HookFn
           
foreign import ignore """
  function ignore (l) {
    return function (node, prop) {
      node.removeEventListener(l.event, l.callback);
    };
  }""" :: Listener -> HookFn
          
hook s act = 
  let l = listener s act
  in vhook {hook: listen l, unhook: ignore l}

-- copied part ended

emptyHook :: VHook
emptyHook = vhook {}


foreign import composeHooks """
function composeHooks(hookOne) {
  return function(hookTwo) {
    var rVHook  = function () { };
    rVHook.prototype.hook = function() {
      hookOne.hook.apply(hookOne, arguments);
      hookTwo.hook.apply(hookTwo, arguments);
    };
    rVHook.prototype.unhook = function() {
      hookOne.unhook.apply(hookOne, arguments);
      hookTwo.unhook.apply(hookTwo, arguments);
    };
    
    return new rVHook;
  };
}
""" :: VHook -> VHook -> VHook
