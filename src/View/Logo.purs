module View.Logo where

import Control.Monad.Eff
import View.Shortcuts (a)
import VirtualDOM.VTree (VTree(), vtext)
import qualified Config as Config

-- | send and st will be removed
view :: forall a b e. a -> b -> Eff e VTree
view send st = return $ 
  a {"className": "navbar-brand",
     "href": Config.slamDataHome} [
    vtext "SlamData"
    ]




