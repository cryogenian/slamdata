module Control.Reactive.File where

import VirtualDOM.VTree
import VirtualDOM
import Utils
import VirtualDOM.Events
import Control.Apply
import Control.Monad.Eff
import Control.Reactive.Event
import Data.DOM.Simple.Element
import Data.DOM.Simple.Types
import DOM
import Data.Maybe


-- prependFileUploader "li" {} [vtext "foo"] = vnode "li" {} [input {type: "file"}]

tst :: Event -> Eff _ Unit
tst evt = do
  mbInput <- target evt >>= querySelector "input"
  case mbInput of
    Nothing -> return unit
    Just input -> void $ raiseEvent "click" input
      

uploader :: forall p a. String -> {click::VHook|p} -> [VTree]  -> VTree
uploader name props children = 
  vnode name props{"click" = hook "click" tst `composeHooks` props.click} $ 
  (vnode "input" {"type": "file", "style": {"display": "none"}} []):children
  
  
