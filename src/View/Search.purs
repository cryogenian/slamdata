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

import qualified Hash as Hash

data Action = Init | Change String | HashChanged String | Submit

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
          "onsubmit": emit Submit} [
      div {"className": "input-group" <> if st.valid then "" else " has-error",
           "style": {"width": "100%"}} [
         span {"className": "input-group-addon"} [
            vtext "Path:"
            ],
         input {"className": "form-control",
                "type": "text",
                "value": st.value,
                "oninput": mkCallback $ (\node event -> do
                                            val <- getValue node
                                            emit $ Change val)
                
               } [],
       span {"className": "input-group-btn"} [
         button {"className": "btn btn-default" <> if st.valid
                                                   then ""
                                                   else " btn-danger",
                 "type": "submit",
                 "onclick": emit Submit} [
            i {"className": "glyphicon glyphicon-search"} []
            ]
         ]
         ]
      ]
    

foldState :: Action -> State -> Eff _ State
foldState action state = do 
  case action of 
    Init -> return state
    HashChanged hash -> do
      return state{value = hash}
    Change val -> do
      case state.timeout of
        Nothing -> return unit
        Just t -> clearTimeout t
      newTimeout <- timeout waitUntilSettingHash $ Hash.setHash val
      return state{timeout = Just newTimeout, value = val}
    Submit -> do
      Hash.setHash state.value
      return state
