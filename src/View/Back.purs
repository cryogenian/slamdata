-- | This component will not be rendered alone, so, it has not a spec
module View.Back where

import Control.Monad.Eff
import Debug.Trace (Trace())
import DOM (DOM())
import View.Shortcuts (i, a, jsVoid)
import VirtualDOM.VTree (VTree())
import VirtualDOM.Events (hook)
import Component (Receiver())
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

view :: forall e. Receiver Action (dom::DOM|e) -> State -> Eff (dom::DOM|e) VTree
view send st = do
  let onClicked _ = do
        Router.setPath ""
        
  return $ a {"className": "navbar-brand",
              "href": jsVoid,
              "click": hook "click" onClicked} [

    viewIcon st 
    ]


foldState :: forall e. Action -> State -> Eff e State
foldState action state = 
  case action of
    Init -> return state
    Changed st -> return st


                              
