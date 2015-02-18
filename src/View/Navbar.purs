module View.Navbar where

import DOM
import View.Shortcuts
import Utils
import Signal
import Signal.Channel
import Signal.Effectful
import VirtualDOM
import VirtualDOM.VTree
import Control.Monad.Eff
import Data.Maybe
import Data.Monoid
import Control.Timer
import VirtualDOM.Events
import Component

import qualified View.Search as Search
import qualified View.Back as Back
import qualified View.Logo as Logo
import qualified View.User as User
import qualified Hash as Hash


type State = {
  search :: Search.State,
  back :: Back.State,
  user :: User.State
  }
initialState :: State
initialState = {
  search: Search.initialState,
  back: Back.initialState,
  user: User.initialState
  }

data Action = Init | SearchAction Search.Action | BackAction Back.Action

view :: Receiver Action _ -> State -> Eff _ VTree
view send st = do
  logo <- Logo.view toVoid {}
  back <- Back.view (\x -> send $ BackAction x) st.back
  search <- Search.view (\x -> send $ SearchAction x) st.search
  user <- User.view toVoid st.user

  return $ nav {"className": "navbar navbar-inverse navbar-fixed-top"} [
    div {"className": "container"} [
       div {"className": "col-sm-3"} [
          back,
          logo
          ],
       div {"className": "col-sm-6"} [
         search
         ],
       div {"className": "col-sm-3"} [
         user
         ]
       ]
    ]

foldState :: Action -> State -> Eff _ State
foldState action state =
  case action of
    Init -> return state
    SearchAction action -> do
      searchState <- Search.foldState action state.search
      return state{search = searchState}
    BackAction action -> do
      backState <- Back.foldState action state.back
      return state{back = backState}

hook :: Receiver Action _ -> Eff _ Unit
hook receiver = do
  hashComp <- Hash.construct
  runSignal $ hashComp.signal ~> \hash ->
    receiver $ (SearchAction <<< Search.HashChanged $ hash)
