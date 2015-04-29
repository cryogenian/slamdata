-- | Shortcuts and glue between different modules
module Utils where

import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Aff
import Data.Maybe
import Debug.Trace
import Debug.Foreign
import Control.Apply
import Data.Function

import DOM (DOM())
import Data.DOM.Simple.Types (HTMLElement(), DOMEvent(), DOMLocation())
import Data.DOM.Simple.Element (appendChild)
import Data.DOM.Simple.Document (body)
import Data.DOM.Simple.Window (globalWindow, document, getLocation, location)
import Data.DOM.Simple.Events (addUIEventListener, UIEventType(..))
import qualified Data.String as Str
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

-- | Opens url in new tab or window
foreign import newTab
  """
  function newTab(url) {
    return function() {
      window.open(url, "_blank");
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
bodyHTMLElement = document globalWindow >>= body

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
locationString = do
  location globalWindow >>= locationOrigin


trimQuotes :: String -> String
trimQuotes input = Rgx.replace start "" $ Rgx.replace end "" input
  where start = Rgx.regex "^\"" Rgx.noFlags
        end = Rgx.regex "\"$" Rgx.noFlags

endsWith :: String -> String -> Boolean
endsWith needle haystack =
  Str.indexOf' needle (Str.length haystack - Str.length needle) haystack /= -1
