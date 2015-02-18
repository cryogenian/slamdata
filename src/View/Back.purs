-- | This component will not be rendered alone, so, it has not a spec
module View.Back where

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

data Action = Init | Clicked | Changed State

data State = Directory | Database | Table | Notebook

initialState = Notebook

viewIcon :: State -> VTree
viewIcon st =
  case st of
    Directory -> i {"className": "glyphicon glyphicon-folder-open"} []
    Database -> i {"className": "glyphicon glyphicon-hdd"} []
    Notebook -> i {"className": "glyphicon glyphicon-list-alt"} []
    Table -> i {"className": "glyphicon glyphicon-th"} []

view :: Receiver Action _ -> State -> Eff _ VTree
view send st = do
  return $ a {"className": "navbar-brand",
              "href": jsVoid,
              "onclick": send Clicked} [
    viewIcon st 
    ]


foldState :: Action -> State -> Eff _ State
foldState action state =
  case action of
    Init -> return state
    Clicked -> do
      alert "clicked"
      return state
    Changed st -> return st


