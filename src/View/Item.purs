-- | This component won't be rendered alone, it hasn't spec
module View.Item where

import Control.Monad.Eff
import Component
import VirtualDOM
import VirtualDOM.VTree
import View.Shortcuts
import VirtualDOM.Events
import Data.Monoid
import Utils
import Signal
import Signal.Effectful
import Signal.Channel

data ResourceType = File | Database | Notebook | Directory | Table 

type Logic = {
  resource :: ResourceType,
  name :: String,
  id :: String 
  }

type State = {
  logic :: Logic,
  isSelected :: Boolean,
  isHovered :: Boolean
  }
data Action = Init
            | Focus | Blur | Open | Activate | Unactivate
            | Configure Logic | Trash Logic | Share Logic 
                                                      

initialState :: State
initialState = {
  isSelected: false,
  isHovered: false,
  logic: {
    name: "",
    id: "",
    resource: File
    }}

renderResourceType :: ResourceType -> VTree
renderResourceType rt = 
  case rt of
    Directory -> i {"className": "glyphicon glyphicon-folder-open"} []
    Database -> i {"className": "glyphicon glyphicon-hdd"} []
    Notebook -> i {"className": "glyphicon glyphicon-list-alt"}[]
    Table -> i {"className": "glyphicon glyphicon-th"}[]
    File -> i {"className": "glyphicon glyphicon-file"}[]

renderMiniToolbar :: Receiver Action _ -> State -> Eff _ [VTree]
renderMiniToolbar send st = do
  let basic = [
        li {} [a {"href": jsVoid,
                  "click": hook "click" $ const (send $ Trash st.logic)}
               [vtext "trash"]],
        li {} [a {"href": jsVoid,
                  "click": hook "click" $ const (send $ Share st.logic)}
               [vtext "share"]]
        ]
  let configure =
        li {} [a {"href": jsVoid,
                  "click": hook "click" $ const (send $ Configure st.logic)}
               [vtext "configure"]]
  case st.logic.resource of
    Database -> return $ configure:basic
    _ -> return basic

view :: Receiver Action _ -> State -> Eff _ VTree
view send st = do
  toolbarItems <- renderMiniToolbar send st
  return $ div {"className": "list-group-item" <> isActive st,
                "mouseout": hook "mouseout" $ const $ (send Blur),
                "mouseover": hook "mouseover" (const $ send Focus),
                "dblclick": hook "dblclick" (const $ send Open),
                "click": hook "click" (const $ send Activate)
                } [
    div {"className": "row"} [
       div {"className": "col-sm-6"} [
          a {"click": hook "click" (const $ send Open),
             "href": jsVoid} [
             renderResourceType st.logic.resource,
             span {style: {"margin-left": "10px"}} [vtext st.logic.name]
             ]
          ],
     div {"className": "col-sm-6" <> hideBlured st} [
         ul {"className": "list-inline pull-right",
             "style": {
               "margin-bottom": 0
               }} toolbarItems
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
foldState (Share l) state = do
  return state
foldState (Configure l) state = do
  return state
foldState (Trash l) state = do
  return state

