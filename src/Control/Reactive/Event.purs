-- | Custom event module. Probably would be moved to ```simple-dom```
-- | or other package
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

foreign import detail """
function detail(ev) {
  return function() {
    return ev.detail;
  };
}
""" :: forall e a. Event -> Eff e a


foreign import raiseEventImpl """
function raiseEventImpl(name, node, content) {
  return function() {
    var evt = new CustomEvent(name, {detail: content});
    evt.initEvent(name, true, true);
    node.dispatchEvent(evt);
    return node;
  };
}
""" :: forall e a. Fn3 String HTMLElement a (Eff (dom::DOM|e) HTMLElement)

raiseEvent :: forall e a. String -> HTMLElement -> a -> Eff (dom::DOM|e) HTMLElement
raiseEvent name el content = runFn3 raiseEventImpl name el content





