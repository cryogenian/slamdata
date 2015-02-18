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


