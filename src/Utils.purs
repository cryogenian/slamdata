-- | Shortcuts and glue between different modules
module Utils where

import Control.Monad.Eff
import Data.Maybe
import Debug.Trace 
import Debug.Foreign
import Control.Apply
import Data.Function

import DOM (DOM(), Node())
import Data.DOM.Simple.Types (HTMLElement(), DOMEvent())
import Data.DOM.Simple.Document (body)
import Data.DOM.Simple.Window (globalWindow, document)
import Data.DOM.Simple.Events (addUIEventListener, UIEventType(..))




-- | It's simpler for me to use foreign logging
-- | then add Show instances
log :: forall a e. a -> Eff (trace::Trace|e) Unit 
log a = fprint a *> pure unit

-- | Ok, I were was wrong, it's simplier to define onload
-- | by simple-dom
onLoad :: forall e. Eff (dom::DOM|e) Unit -> Eff (dom::DOM|e) Unit
onLoad action = do
  let handler :: DOMEvent -> _
      handler _ = action
  addUIEventListener LoadEvent handler globalWindow

-- | append one element to another element
-- | PR to simple-dom
foreign import append """
function append(parent) {
  return function(child) {
    return function() {
      parent.appendChild(child);
      return parent;
    };
};
}
""" :: forall e. HTMLElement -> HTMLElement -> Eff (dom::DOM|e) HTMLElement


foreign import parentImpl """
function parentImpl(nothing, just, child) {
  return function() {
    var p = child.parentElement;
    if (!p) return nothing;
    return just(p);
  };
}
""" :: forall e a.
       Fn3 (Maybe a) (a -> Maybe a) HTMLElement (Eff e (Maybe HTMLElement))

-- | get parent of element
-- | PR to simple-dom
parent :: forall e. HTMLElement -> Eff e (Maybe HTMLElement)
parent = runFn3 parentImpl Nothing Just 

-- | need to PR to simple-dom
foreign import hashChanged """
function hashChanged(callback) {
  return function() {
    callback(location.hash)("")();
    window.addEventListener("hashchange", function(ev) {
      callback(ev.newURL)(ev.oldUrl)();
    });
  };
}
""" :: forall a e. (String -> String -> Eff e Unit) -> Eff e Unit

-- | This function is needed to convert VTree -> Node -> HTMLElement 
foreign import convertToElement """
function convertToElement(a) {return a;}
""" :: Node -> HTMLElement


-- | Shortcut
bodyNode = do
  document globalWindow >>= body
  

