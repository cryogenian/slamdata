module VirtualDOM.Events where

import Control.Monad.Eff
import DOM
import Data.DOM.Simple.Types
import Data.DOM.Simple.Element
import Utils

foreign import data Event :: *
foreign import data Callback :: *

type Handler e = Node -> Event -> Eff e Unit 

foreign import mkCallback """
function mkCallback(handler) {
  return function(event) {
    handler(event.target)(event)();
  };
}
""" :: forall e.  Handler e -> Callback


getValue :: forall e. Node -> Eff (dom::DOM|e) String 
getValue = value <<< convertToElement

foreign import returnFalse """
function returnFalse() {
  return false;
}
""" :: Callback
