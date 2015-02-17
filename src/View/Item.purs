module View.Item where

import Control.Monad.Eff
import Component
import VirtualDOM
import VirtualDOM.VTree
import View.Shortcuts
import Data.Monoid
import Utils
import Signal
import Signal.Effectful
import Signal.Channel

type State = {
  isSelected :: Boolean,
  isHovered :: Boolean
  }
data Action = Init | Focus | Blur | Open | Activate | Unactivate

initialState :: State
initialState = {isSelected: false, isHovered: false}

view :: Receiver Action _ -> State -> Eff _ VTree
view send st = do
  return $ div {"className": "list-group-item" <> isActive st,
                "onclick": send Activate,
                "ondblclick": send Open,
                "onmouseover": send Focus,
                "onmouseout": send Blur
                } [
    div {"className": "row"} [
       div {"className": "col-sm-6"} [
          a {"onclick": send Open,
             "href": jsVoid} [
             i {"className": "glyphicon glyphicon-hdd"} [],
             span {style: {"margin-left": "10px"}} [vtext "Resource name"]
             ]
          ],
     div {"className": "col-sm-6" <> hideBlured st} [
         ul {"className": "list-inline pull-right",
             "style": {
               "margin-bottom": 0
               }} [
            li {} [a {"href": jsVoid} [vtext "configure"]],
            li {} [a {"href": jsVoid} [vtext "trash"]],
            li {} [a {"href": jsVoid} [vtext "share"]]
            ]
         ]
       ]
    ]
  where isActive st = if st.isSelected then
                        " list-group-item-info"
                        else ""
        hideBlured st = if st.isHovered || st.isSelected then
                          ""
                          else " hidden"
foldState :: Action -> State -> Eff _ State
foldState Focus state = do
  return state{isHovered = true}
foldState Blur state = do
  return state{isHovered = false}
foldState Activate state = do
  return state{isSelected = true}
foldState Unactivate state = do
  return state{isSelected = false}
foldState Open state = do
  return state
foldState Init state = do
  return state


