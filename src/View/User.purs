module View.User where

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
newtype State = State String
initialState = State "Maxim Zimaliev"

view :: Receiver Action _ -> State -> Eff _ VTree
view send (State st) = do
  return $ ul {"className": "nav navbar-nav navbar-right"} [
    li {} [
       a {"href": jsVoid} [
          vtext st,
          i {"className": "glyphicon glyphicon-user"} []
          ]
       ]
    ]

foldState :: Action -> State -> Eff _ State
foldState _ state = return state

foldAll :: Receiver Action _ -> Action -> Folder State -> Eff _ (Folder State)
foldAll send action all = return all 

construct :: Eff _ (Component Action State)
construct = do
  chan <- channel Init
  vt <- view (send chan) initialState
  signal <- foldpE (foldAll (send chan)) (mkFolder initialState) (subscribe chan)
  return {
    signal: signal,
    channel: chan,
    vt: vt
    }
                  
               
