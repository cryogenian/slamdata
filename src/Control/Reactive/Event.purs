module Control.Reactive.Event where

import Data.DOM.Simple.Types
import Control.Monad.Eff
import Data.Function
import DOM

foreign import data Event :: *

foreign import target """
function target(ev) {
  return function() {
    return ev.target;
  };
}
""" :: forall e. Event -> Eff e HTMLElement

foreign import raiseEventImpl """
function raiseEventImpl(name, node) {
  return function() {
    var evt = new Event(name);
    evt.initEvent(name, true, true);
    node.dispatchEvent(evt);
    return node;
  };
}
""" :: forall e. Fn2 String HTMLElement (Eff (dom::DOM|e) HTMLElement)

raiseEvent :: forall e. String -> HTMLElement -> Eff (dom::DOM|e) HTMLElement
raiseEvent name el = runFn2 raiseEventImpl name el



