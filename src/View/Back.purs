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

data Action = Init | Clicked | Changed State | Ajax XHR.Output

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
              "click": hook "click" $ const (send Clicked)} [

    viewIcon st 
    ]


foldState :: Action -> State -> Eff _ State
foldState action state = 
  case action of
    Init -> return state
    Clicked -> do
{-      xhr <- XHR.construct
      xhr.run (foldState  {
        method: A.GET,
        content: A.NoData,
        additionalHeaders: (empty :: StrMap String),
        url: "http://localhost:5050/"
        }
-}
      -- just to be sure that we catch this click
      log "clicked"
      return state
    Changed st -> return st


hookFn :: forall e.
        Receiver Action (chan::Chan,dom::DOM,trace::Trace|e) -> 
        Eff (chan::Chan,dom::DOM,trace::Trace|e) Unit
hookFn receiver = do 
  return unit
                              
