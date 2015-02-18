-- | Most of these functions will be removed
module Utils where

import Control.Monad.Eff
import DOM
import Data.DOM.Simple.Types
import Data.DOM.Simple.Document
import Data.DOM.Simple.Window
import Data.DOM.Simple.Element
import Data.Maybe


-- | It's simpler for me to use foreign logging
-- | then add Show instances
foreign import log """
function log(a) {
  return function() {
    console.log(a);
  };
}
""" :: forall a e. a -> Eff e Unit

-- | Used for mocking somewhere :)
foreign import alert """
function alert(str) {
  return function() {
    window.alert(str);
  };
}
""" :: forall e. String -> Eff e Unit

-- | This function can be defined via
-- | purescript-simple-dom but it will be
-- | harder to understand
foreign import onLoad """
function onLoad(action) {
  return function( ){
    window.addEventListener("load", function() {
      action();
    });
  };
}
""" :: forall e. Eff e Unit -> Eff e Unit

-- | append one node to another node
-- | need to rewrite to HTMLElement
-- | and make PR to simple-dom
foreign import append """
function append(parent) {
  return function(child) {
    return function() {
      parent.appendChild(child);
      return parent;
    };
  };
}
""" :: forall e. Node -> Node -> Eff (dom::DOM|e) Node

-- | need to PR to simple-dom
foreign import hashChanged """
function hashChanged(callback) {
  return function() {
    window.addEventListener("hashchange", function(ev) {
      callback(ev.newURL)();
    });
  };
}
""" :: forall a e. (String -> Eff e Unit) -> Eff e Unit

-- | Will be removed
foreign import convertToNode """
function convertToNode(a) {return a;}
""" :: HTMLElement -> Node
-- | Will be removed
foreign import convertToElement """
function convertToElement(a) {return a;}
""" :: Node -> HTMLElement

-- | Shortcut : will be removed
appendToId :: String -> Node -> Eff _ (Maybe Node)
appendToId id node = do
  mbEl <- document globalWindow >>= getElementById id
  case mbEl of
    Nothing -> return Nothing
    Just el -> do
      el <- append (convertToNode el) node
      return $ Just el

-- | Shortcut
appendToBody :: Node -> Eff _ Node
appendToBody node = do
  bd <- document globalWindow >>= body
  append (convertToNode bd) node

-- | Shortcut
nodeById :: String -> Eff _ (Maybe Node)
nodeById id = do
  mbEl <- document globalWindow >>= getElementById id
  return $ convertToNode <$> mbEl

-- | Shortcut
bodyNode :: Eff _ Node
bodyNode = do
  convertToNode <$> (document globalWindow >>= body)
  
-- | Will be removed (I've seen this in updates of purescript-strings)
emptyFlags = {
  unicode: false,
  sticky: false,
  global: false,
  multiline: false,
  ignoreCase: false
  }
