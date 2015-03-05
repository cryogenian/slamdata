-- | This component also has no spec, and will not be rendered alone
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
import Control.Monad.Eff.Ref
import Data.Maybe
import Data.Monoid
import Control.Timer
import VirtualDOM.Events hiding (hook)
import Component

import qualified View.Search as Search
import qualified View.Back as Back
import qualified View.Logo as Logo
import qualified View.User as User
import qualified Router as Router

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
data Action = Init | SearchAction Search.Action | BackAction Back.Action

-- | Render
view :: Receiver Action _ -> State -> Eff _ VTree
view send st = do
  -- rendering children
  -- we provide them receiver as function builded from Action-constructor
  -- and state as State fields
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

-- | Update state
foldState :: Action -> State -> Eff _ State
foldState action state =
  case action of
    -- Inner message
    Init -> return state
    -- Children messages
    SearchAction action -> do
      -- update substate by child updater
      searchState <- Search.foldState action state.search
      return state{search = searchState}
    BackAction action -> do
      -- update substate by child updater
      backState <- Back.foldState action state.back
      return state{back = backState}


-- | listen route changes, called after render
hookFn :: forall e.
        Receiver Action _ -> 
        Eff _ Unit
hookFn receiver = do
  Back.hookFn (receiver <<< BackAction)
  Hash.changed $ do
    route <- Router.getRoute
    log route
    receiver $ (SearchAction <<< Search.RouteChanged $ route.search)


