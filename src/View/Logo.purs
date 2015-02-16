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

data Action = Init
type State = {}

view :: Receiver Action _ -> State -> Eff _ VTree
view send st = return $ 
  a {"className": "navbar-brand",
     "href": jsVoid} [
    vtext "SlamData"
    ]

foldState :: Action -> State -> Eff _ State
foldState action state = return state

foldAll :: Receiver Action _ -> Action -> Folder State -> Eff _ (Folder State)
foldAll send action all = return all

construct :: Eff _ (Component Action State)
construct = do
  chan <- channel Init
  vt <- view (send chan) {}
  signal <- foldpE (foldAll (send chan)) (mkFolder {}) (subscribe chan)
  return {
    signal: signal,
    channel: chan,
    vt: vt
    }
