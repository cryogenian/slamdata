-- | This component is not component :)
-- | It is just two function and even have not action and state
module View.Logo where

import DOM
import View.Shortcuts
import Utils
import Signal
import Signal.Channel
import Signal.Effectful
import VirtualDOM
import VirtualDOM.VTree
import Control.Monad.Eff
import VirtualDOM.Events
import Component

-- | send and st will be removed
view :: forall a b. a -> b -> Eff _ VTree
view send st = return $ 
  a {"className": "navbar-brand",
     "href": jsVoid} [
    vtext "SlamData"
    ]




