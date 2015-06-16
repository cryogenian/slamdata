-- | Shortcuts and glue between different modules
module Utils where

import Control.Apply ((*>))
import Control.Monad.Eff (Eff())
import Data.DOM.Simple.Document (body)
import Data.DOM.Simple.Element (appendChild)
import Data.DOM.Simple.Events (addUIEventListener, UIEventType(..))
import Data.DOM.Simple.Types (HTMLElement(), DOMEvent(), DOMLocation())
import Debug.Foreign (fprint)
import Debug.Trace (Trace())
import Data.Maybe (Maybe(..))
import Data.Array (elemIndex)
import DOM (DOM())
import Global (readFloat, isNaN, readInt)

import qualified Data.String as Str
import qualified Data.String.Regex as Rgx
import qualified Data.DOM.Simple.Window as W

log :: forall a e. a -> Eff (trace::Trace|e) Unit
log a = fprint a *> pure unit

onLoad :: forall e. Eff (dom::DOM|e) Unit -> Eff (dom::DOM|e) Unit
onLoad action = do
  let handler :: DOMEvent -> _
      handler _ = action
  addUIEventListener LoadEvent handler W.globalWindow

-- | Opens url in new tab or window
foreign import newTab
  """
  function newTab(url) {
    return function() {
      window.open(url, "_blank");
    };
  }
  """ :: forall e. String -> Eff (dom :: DOM | e) Unit

foreign import mailOpen """
function mailOpen(url) {
  return function() {
    window.open(url);
  };
}
""" :: forall e. String -> Eff (dom :: DOM | e) Unit

foreign import reload
  """
  function reload() {
    document.location.reload();
  }
  """ :: forall e. Eff (dom :: DOM|e) Unit

foreign import clearValue
  """
  function clearValue(el) {
    return function() {
      el.value = null;
    };
  }
  """ :: forall e. HTMLElement -> Eff (dom :: DOM | e) Unit

foreign import select
  """
  function select(node) {
    return function() {
      node.select();
    };
  }
  """ :: forall e. HTMLElement -> Eff (dom :: DOM | e) Unit

bodyHTMLElement :: forall e. Eff (dom :: DOM | e) HTMLElement
bodyHTMLElement = W.document W.globalWindow >>= body

mountUI :: forall e. HTMLElement -> Eff (dom :: DOM | e) Unit
mountUI node = bodyHTMLElement >>= flip appendChild node

foreign import locationOrigin
  """
  function locationOrigin(loc) {
    return function() {
      return loc.origin;
    }
  }
  """ :: forall e. DOMLocation -> Eff (dom :: DOM|e) String

locationString :: forall e. Eff (dom :: DOM |e) String
locationString = W.location W.globalWindow >>= locationOrigin

endsWith :: String -> String -> Boolean
endsWith needle haystack =
  Str.indexOf' needle (Str.length haystack - Str.length needle) haystack /= -1

setLocation :: forall e. String -> Eff (dom :: DOM | e) Unit
setLocation url = W.location W.globalWindow >>= W.setLocation url

foreign import replaceLocation
  """
  function replaceLocation(url) {
    return function() {
      window.location.replace(url);
    };
  }
  """ :: forall e. String -> Eff (dom :: DOM | e) Unit

s2i :: String -> Maybe Number
s2i s =
  let n = readInt 10 s in
  if isNaN n
  then Nothing
  else Just n

s2n :: String -> Maybe Number
s2n s =
  let n = readFloat s in
  if isNaN n
  then Nothing
  else Just n


elem :: forall a. (Eq a) => a -> [a] -> Boolean
elem a lst = elemIndex a lst /= -1
