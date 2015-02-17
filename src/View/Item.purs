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
  log st.isSelected
  return $ div {"className": "list-group-item" <> isActive st} [
    div {"className": "row",
         "ondblclick": send Open,
         "onclick": send Activate,
         "onmouseover": send Focus,
         "onmouseout": send Blur} [
       div {"className": "col-sm-6"} [
          i {"className": "glyphicon glypicon-stop"} [],
          a {"onclick": send Open,
             "href": jsVoid} [vtext "Resource name"]
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


foldAll :: Receiver Action _ -> Action -> Folder State -> Eff _ (Folder State)
foldAll receiver action {state: state, current: current, previous: previous} = do
  new <- foldState action state
  newVt <- view receiver new
  return {state: new, previous: current, current: newVt}

construct :: Eff _ (Component Action State)
construct = do
  chan <- channel Init
  vt <- view (send chan) initialState
  let folder = mkFolder initialState
  signal <- foldpE (foldAll (send chan)) folder (subscribe chan)

  return $ {
    signal: signal,
    channel: chan,
    vt: emptyVTree
    }
