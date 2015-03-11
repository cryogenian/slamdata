-- | This component also has no spec, and will not be rendered alone
module View.Navbar where

import Data.Maybe
import Control.Monad.Eff

import DOM (DOM())
import View.Shortcuts (div, emptyVTree, nav)
import Signal.Channel (Chan())
import VirtualDOM.VTree (VTree())
import Control.Timer (Timer())
import VirtualDOM.Events hiding (hook)
import Component (Receiver(), toVoid)

import qualified View.Search as Search
import qualified View.Back as Back
import qualified View.Logo as Logo
import qualified View.User as User
import qualified Router as Router
import qualified Config as Config

-- | Multiplication of children state
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

-- | Sum of children actions
data Action = Init
            | SearchAction Search.Action
            | BackAction Back.Action

-- | Render
view :: forall e.
        Receiver Action (chan::Chan, timer::Timer, dom::DOM|e) -> State ->
        Eff (chan::Chan, timer::Timer, dom::DOM|e) VTree
view send st = do
  logo <- Logo.view toVoid {}
  back <- Back.view (\x -> send $ BackAction x) st.back
  search <- Search.view (\x -> send $ SearchAction x) st.search
  user <- if Config.userEnabled then
            User.view toVoid st.user
            else return emptyVTree

  return $ nav {"className": "navbar navbar-inverse navbar-fixed-top"} [
    div {"className": "container"} [
       div {"className": "col-sm-3"} [
          back,
          logo
          ],
       div {"className": "col-sm-7"} [
         search
         ],
       div {"className": "col-sm-2"} [
         user
         ]
       ]
    ]

-- | Update state
foldState :: forall e. Action -> State -> Eff e State
foldState action state =
  case action of
    Init -> return state
    SearchAction action -> do
      searchState <- Search.foldState action state.search
      return state{search = searchState}
    BackAction action -> do
      backState <- Back.foldState action state.back
      return state{back = backState}


-- | listen route changes, called after inserting in DOM
hookFn :: forall e.
        Receiver Action (dom::DOM, chan::Chan|e) -> 
        Eff (dom::DOM, chan::Chan|e) Unit
hookFn receiver = do
  Hash.changed $ do
    route <- Router.getRoute
    receiver $ (SearchAction <<< Search.RouteChanged $ route.search)


