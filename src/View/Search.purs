-- | Search component will not be rendered alone
module View.Search where

import Control.Monad.Eff
import Data.Maybe
import Data.Either

import DOM (DOM())
import View.Shortcuts (div, input, span, button, form, i)
import VirtualDOM.VTree (VTree(), vtext)
import Control.Timer (Timer(), Timeout(), clearTimeout, timeout)
import VirtualDOM.Events (hook)
import Component (Receiver(..))
import Control.Reactive.Event (Event(..), target)
import Data.DOM.Simple.Element (value)
import Text.SlamSearch.Parser (parseSearchQuery)
import Signal.Channel (Chan())

import qualified Hash as Hash
import qualified Router as Router
import qualified Config as Config

-- | Route change is external message
data Action = Init | Change Timeout String | RouteChanged String | Disable | Enable

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
    timeout: Nothing
  }

changeHandler :: forall e. Receiver Action (chan::Chan, dom::DOM, timer::Timer|e) -> 
                 State -> Event -> Eff (chan::Chan, dom::DOM, timer::Timer|e) Unit
changeHandler sendBack state evt = do 
  val <- target evt >>= value
  case state.timeout of
    Nothing -> return unit
    Just t -> clearTimeout t
  newTimeout <- timeout Config.searchTimeout $ do
    submitHandler sendBack state{value = val}
  sendBack $ Change newTimeout val

submitHandler :: forall e. Receiver Action (chan::Chan, dom::DOM|e) -> 
                 State -> Eff (chan::Chan, dom::DOM|e) Unit
submitHandler sendBack state = do
  case parseSearchQuery state.value of
    Left _ -> sendBack Disable
    Right _ -> do
      Router.setSearch state.value
      sendBack Enable

view :: forall e. Receiver Action (chan::Chan, dom::DOM, timer::Timer|e) -> 
        State -> Eff (chan::Chan, dom::DOM, timer::Timer|e) VTree
view emit st = do
  return $ form {"className": "navbar-form",
          "submit": hook "submit" $ const $ (submitHandler emit st) } [
      div {"className": "input-group" <> if st.valid then "" else " has-error",
           "style": {"width": "100%"}} [
         span {"className": "input-group-addon"} [
            vtext "Path:"
            ],
         input {"className": "form-control",
                "type": "text",
                "value": st.value,
                "input": hook "input" (changeHandler emit st) 
               } [],
       span {"className": "input-group-btn"} [
         button {"className": "btn btn-default" <> if st.valid
                                                   then ""
                                                   else " btn-danger",
                 "type": "submit",
                 "disabled": not st.valid,
                 "click": hook "click" $ const $ (submitHandler emit st) } [

            i {"className": "glyphicon glyphicon-search"} []
            ]
         ]
         ]
      ]
    
-- | Update searcher state
foldState :: forall e. Action -> State -> Eff e State
foldState action state = do 
  case action of
    Init -> return state
    RouteChanged hash -> do
      return state{value = hash}
    Change timeout val -> do
      return state{timeout = Just timeout, value = val}
    Disable -> return state{valid = false}
    Enable -> return state{valid = true}


