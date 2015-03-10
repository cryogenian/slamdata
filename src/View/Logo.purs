module View.Logo where

import View.Shortcuts
import Signal
import VirtualDOM
import VirtualDOM.VTree
import Control.Monad.Eff
import Config

-- | send and st will be removed
view :: forall a b. a -> b -> Eff _ VTree
view send st = return $ 
  a {"className": "navbar-brand",
     "href": Config.slamDataHome} [
    vtext "SlamData"
    ]




