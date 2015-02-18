-- | Shortcuts for VirtualDOM events
-- | They are not well typed, so it's possible
-- | to put in "onclick" everything
-- | it works when we put  Eff _ Unit there or
-- | Callback defined here
module VirtualDOM.Events where

import Control.Monad.Eff
import DOM
import Data.DOM.Simple.Types
import Data.DOM.Simple.Element
import Utils

foreign import data Event :: *
foreign import data Callback :: *

-- | Will be changed in favor of HTMLElement, I suppose
type Handler e = Node -> Event -> Eff e Unit 

foreign import mkCallback """
function mkCallback(handler) {
  return function(event) {
    handler(event.target)(event)();
  };
}
""" :: forall e.  Handler e -> Callback

-- | Just get value from **input**
getValue :: forall e. Node -> Eff (dom::DOM|e) String 
getValue = value <<< convertToElement

-- | emptyHandler
foreign import returnFalse """
function returnFalse() {
  return false;
}
""" :: Callback
