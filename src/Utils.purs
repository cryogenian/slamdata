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
import qualified Data.String.Regex as Rgx

-- I think that we will be able to switch from `purescript-simple-dom`
-- if we use `affjax`
foreign import encodeURIComponent :: String -> String
foreign import decodeURIComponent :: String -> String

log :: forall a e. a -> Eff (trace::Trace|e) Unit 
log a = fprint a *> pure unit

onLoad :: forall e. Eff (dom::DOM|e) Unit -> Eff (dom::DOM|e) Unit
onLoad action = do
  let handler :: DOMEvent -> _
      handler _ = action
  addUIEventListener LoadEvent handler globalWindow

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

-- | Opens url in new tab or window
foreign import newTab """
function newTab(url) {
  return function() {
    window.open(url, "_blank");
  };
}
""" :: forall e. String -> Eff (dom::DOM|e) Unit 

-- | converts `Node` to `HTMLElement`
foreign import convertToElement """
function convertToElement(a) {return a;}
""" :: Node -> HTMLElement

foreign import reload """
function reload() {
  document.location.reload();
}
""" :: forall e. Eff (dom :: DOM|e) Unit


foreign import clearValue """
function clearValue(el) {
  return function() {
    el.value = null;
  };
}
""" :: forall e. Node -> Eff (dom :: DOM |e) Unit

bodyNode = do
  document globalWindow >>= body
  

trimQuotes :: String -> String
trimQuotes input = Rgx.replace start "" $ Rgx.replace end "" input
  where start = Rgx.regex "^\"" Rgx.noFlags
        end = Rgx.regex "\"$" Rgx.noFlags
