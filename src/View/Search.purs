-- | Search component will not be rendered alone
module View.Search where

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
import Control.Reactive.Event
import Data.DOM.Simple.Element

import qualified Hash as Hash
import qualified Router as Router

-- | Route change is external message
data Action = Init | Change String | RouteChanged String | Submit

type State = {
  valid :: Boolean,
  value :: String,
  timeout :: Maybe Timeout
  }

initialState :: State
initialState =
  {
    valid: true,
    value: "",
    timeout: (Nothing :: Maybe Timeout)
  }


waitUntilSettingHash = 500
  

view :: Receiver Action _ -> State -> Eff _ VTree
view emit st = return $ 
    form {"className": "navbar-form",
          -- on submit we send Submit message to our recevier (emit here)
          -- It will populate in foldState because emit is (send chan)
          "onsubmit": emit Submit} [
      div {"className": "input-group" <> if st.valid then "" else " has-error",
           "style": {"width": "100%"}} [
         span {"className": "input-group-addon"} [
            vtext "Path:"
            ],
         input {"className": "form-control",
                "type": "text",
                "value": st.value,
                -- Here we making callback from effectful function
                -- It sends Change event
                "input": hook "input" $ \event -> do
                  val <- return event >>= target >>= value
                  emit $ Change val
               } [],
       span {"className": "input-group-btn"} [
         button {"className": "btn btn-default" <> if st.valid
                                                   then ""
                                                   else " btn-danger",
                 "type": "submit",
                 -- another submit
                 "onclick": emit Submit} [
            i {"className": "glyphicon glyphicon-search"} []
            ]
         ]
         ]
      ]
    
-- | Update searcher state
foldState :: Action -> State -> Eff _ State
foldState action state = do 
  case action of
    -- just constructed
    Init -> return state
    -- Current route changed - external message
    RouteChanged hash -> do
      return state{value = hash}
    -- User input
    Change val -> do
      -- if there was some input before we will have Just timeout 
      case state.timeout of
        Nothing -> return unit
        Just t -> clearTimeout t
      -- construct timeout that will change route
      newTimeout <- timeout waitUntilSettingHash $ Router.setSearch val
      return state{timeout = Just newTimeout, value = val}
    -- on submit we don't wait
    Submit -> do
      Router.setSearch state.value
      return state

