module Utils where

import Control.Monad.Eff
import DOM
import Data.DOM.Simple.Types
import Data.DOM.Simple.Document
import Data.DOM.Simple.Window
import Data.DOM.Simple.Element
import Data.Maybe



foreign import log """
function log(a) {
  return function() {
    console.log(a);
  };
}
""" :: forall a e. a -> Eff e Unit

foreign import alert """
function alert(str) {
  return function() {
    window.alert(str);
  };
}
""" :: forall e. String -> Eff e Unit

foreign import onLoad """
function onLoad(action) {
  return function( ){
    window.addEventListener("load", function() {
      action();
    });
  };
}
""" :: forall e. Eff e Unit -> Eff e Unit

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

foreign import hashChanged """
function hashChanged(callback) {
  return function() {
    window.addEventListener("hashchange", function(ev) {
      callback(ev.newURL)();
    });
  };
}
""" :: forall a e. (String -> Eff e Unit) -> Eff e Unit


foreign import convertToNode """
function convertToNode(a) {return a;}
""" :: HTMLElement -> Node
foreign import convertToElement """
function convertToElement(a) {return a;}
""" :: Node -> HTMLElement


appendToId :: String -> Node -> Eff _ (Maybe Node)
appendToId id node = do
  mbEl <- document globalWindow >>= getElementById id
  case mbEl of
    Nothing -> return Nothing
    Just el -> do
      el <- append (convertToNode el) node
      return $ Just el

appendToBody :: Node -> Eff _ Node
appendToBody node = do
  bd <- document globalWindow >>= body
  append (convertToNode bd) node

nodeById :: String -> Eff _ (Maybe Node)
nodeById id = do
  mbEl <- document globalWindow >>= getElementById id
  return $ convertToNode <$> mbEl

bodyNode :: Eff _ Node
bodyNode = do
  convertToNode <$> (document globalWindow >>= body)
  
