module Driver.ZClipboard where

import Control.Monad.Eff (Eff())
import Data.DOM.Simple.Element
import Data.DOM.Simple.NodeList
import Data.DOM.Simple.Types (HTMLElement())
import Data.Foldable (for_)
import DOM (DOM())

import qualified Control.UI.ZClipboard as Z

initZClipboard :: forall e. HTMLElement -> Eff (zClipboard :: Z.ZCLIPBOARD, dom :: DOM | e) Unit
initZClipboard node = do
  els <- querySelectorAll "[data-zclipboard]" node >>= nodeListToArray
  for_ els \el -> do
    value <- getAttribute "data-zclipboard" el
    Z.make el >>= Z.onCopy (Z.setData "text/plain" value)
