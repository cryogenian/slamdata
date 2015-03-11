-- | This component won't be rendered alone, it hasn't spec
module View.Item where

import Control.Monad.Eff
import Component (Receiver())
import VirtualDOM.VTree (VTree(), vtext)
import View.Shortcuts (li, a, span, div, jsVoid, i, ul)
import VirtualDOM.Events (hook)
import Signal.Channel (Chan())
import Model (Mount(..), ItemLogic(..), Sort(..))
import qualified Api.Fs as Fs
import DOM (DOM())


type State = {
  logic :: ItemLogic,
  isSelected :: Boolean,
  isHovered :: Boolean
  }
data Action = Init
            | Focus | Blur | Open | Activate | Unactivate
            | Configure ItemLogic | Trash ItemLogic | Share ItemLogic
                                              
                                                      

fromMetadata :: Fs.Metadata -> State
fromMetadata (Fs.Metadata meta) = {
  isSelected: false,
  isHovered: false,
  logic: {
    name: meta.name,
    resource: meta.mount
    }
  }

sort :: Sort -> State -> State -> Ordering
sort dir a b =
  let project = _.logic >>> _.name
  in if project a == ".." then LT
     else case dir of
       Asc -> compare (project a) (project b)
       Desc -> compare (project b) (project a)

initialState :: State
initialState = {
  isSelected: false,
  isHovered: false,
  logic: {
    name: "",
    resource: File
    }}

upNavState :: State
upNavState = {
  isSelected: false,
  isHovered: false,
  logic: {
    name: "..",
    resource: Directory
    }
  }

renderResourceType :: Mount -> VTree
renderResourceType rt = 
  case rt of
    Directory -> i {"className": "glyphicon glyphicon-folder-open"} []
    Database -> i {"className": "glyphicon glyphicon-hdd"} []
    Notebook -> i {"className": "glyphicon glyphicon-list-alt"}[]
    Table -> i {"className": "glyphicon glyphicon-th"}[]
    File -> i {"className": "glyphicon glyphicon-file"}[]

renderMiniToolbar :: forall e.
                     Receiver Action (chan::Chan, dom::DOM|e) -> State ->
                     Eff (chan::Chan, dom::DOM|e) [VTree]
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
    x | st.logic.name == ".." -> return []
    _ -> return basic


open :: forall e. Receiver Action (chan::Chan,dom::DOM|e) -> State -> 
        Eff (chan::Chan, dom::DOM|e) Unit
open sendBack state = do 
  route <- Router.getRoute
  let name = state.logic.name
      path = Router.extractPath route
  if state.logic.resource == Directory || state.logic.resource == Database then 
    Router.setPath (path <> name <> "/")
    else return unit

    
view :: forall e. Receiver Action (chan::Chan, dom::DOM|e)  -> State -> 
        Eff (dom::DOM, chan::Chan|e) VTree
view send st = do
  toolbarItems <- renderMiniToolbar send st
  return $ div {"className": "list-group-item" <> isActive st,
                "mouseout": hook "mouseout" $ const $ (send Blur),
                "mouseover": hook "mouseover" (const $ send Focus),
                "dblclick": hook "dblclick" (const $ open send st),
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
foldState :: forall e. Action -> State -> Eff e State
foldState action state =
  case action of
    Focus -> return state{isHovered = true}
    Blur -> return state{isHovered = false}
    Activate -> return state{isSelected = true}
    Unactivate -> return state{isSelected = false}
    Open -> return state
    Init -> return state
    _ -> return state

