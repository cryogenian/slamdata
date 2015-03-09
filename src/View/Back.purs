-- | This component will not be rendered alone, so, it has not a spec
module View.Back where

import Debug.Trace (Trace())
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
import qualified XHR as XHR
import qualified Data.DOM.Simple.Ajax as A
import Data.StrMap (empty, StrMap())
import qualified Router as Router

data Action = Init | Changed State

data State = Directory | Database | Table | Notebook 

initialState = Directory

viewIcon :: State -> VTree
viewIcon st =
  case st of
    Directory -> i {"className": "glyphicon glyphicon-folder-open"} []
    Database -> i {"className": "glyphicon glyphicon-hdd"} []
    Notebook -> i {"className": "glyphicon glyphicon-list-alt"} []
    Table -> i {"className": "glyphicon glyphicon-th"} []

view :: Receiver Action _ -> State -> Eff _ VTree
view send st = do
  let onClicked _ = do
        Router.setPath ""
        
  return $ a {"className": "navbar-brand",
              "href": jsVoid,
              "click": hook "click" onClicked} [

    viewIcon st 
    ]


foldState :: Action -> State -> Eff _ State
foldState action state = 
  case action of
    Init -> return state
    Changed st -> return st


                              
