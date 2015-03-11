-- | This module will be removed, used only for layout
module View.User where

import Control.Monad.Eff
import View.Shortcuts (li, ul, a, jsVoid, i)
import Signal.Channel (Chan())
import VirtualDOM.VTree (VTree(), vtext)
import VirtualDOM.Events (hook)
import Component (Receiver())
import DOM (DOM())

data Action = Init
newtype State = State String
initialState = State "Maxim Zimaliev"

view :: forall e. Receiver Action (dom::DOM, chan::Chan|e) -> State -> 
        Eff (dom::DOM, chan::Chan|e) VTree
view send (State st) = do
  return $ ul {"className": "nav navbar-nav navbar-right"} [
    li {} [
       a {"href": jsVoid} [
          vtext st,
          i {"className": "glyphicon glyphicon-user"} []
          ]
       ]
    ]

foldState :: forall e. Action -> State -> Eff e State
foldState _ state = return state
