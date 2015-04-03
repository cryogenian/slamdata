module Utils.Event (raiseEvent) where

import Data.DOM.Simple.Types (HTMLElement())
import Control.Monad.Eff
import Data.Function (Fn2(), runFn2)
import DOM (DOM())

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
    var evt = new CustomEvent(name)
    evt.initEvent(name, true, true);
    node.dispatchEvent(evt);
    return node;
  };
}
""" :: forall e a. Fn2 String HTMLElement (Eff (dom::DOM|e) HTMLElement)
raiseEvent :: forall e a. String -> HTMLElement -> Eff (dom::DOM|e) HTMLElement
raiseEvent name el = runFn2 raiseEventImpl name el
