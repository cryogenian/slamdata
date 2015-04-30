module View.Notebook (view, HTML()) where

import Data.Maybe (maybe)
import Control.Functor (($>))
import Control.Apply ((*>))
import Data.Inject1 (inj)
import Control.Plus (empty)
import View.Common (contentFluid, navbar, icon, logo, glyph)

import Data.Array ((..), length, zipWith, replicate)
import Model.Notebook (Input(..), State(..), Notebook(..), Cell(..), notebook, notebookCells)
import Model.Notebook.Menu (DropdownItem(), MenuElement(), MenuInsertSignal(..))
import Controller.Notebook (handleMenuSignal, handleSubmitName)
import Model.Path (path2str, parent)
import Data.Int (toNumber, fromNumber, Int())
import Data.String (joinWith)
import EffectTypes (NotebookAppEff())
import Optic.Core ((^.))

import qualified Data.Argonaut.Encode as Ae
import qualified Data.Argonaut.Printer as Ap

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.Themes.Bootstrap3 as B
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Forms as E
import qualified Config as Config
import qualified View.Css as Vc
import qualified View.File.Modal.Common as Vm
import Driver.File (updatePath)

type HTML p e = H.HTML p (E.Event (NotebookAppEff e) Input)

view :: forall p e. State -> HTML p e
view state =
  H.div [ E.onClick (E.input_ CloseDropdowns) ]
  (navigation state <> body state <>  modal state)

navigation :: forall p e. State -> [HTML p e]
navigation state =
  if not state.editable
  then []
  else
    [ navbar
      [ H.div [ A.classes [ Vc.navCont, Vc.notebookNav, B.containerFluid ] ]
        [ icon B.glyphiconBook $ notebookHref state
        , logo 
        , name state ]
      , H.ul [ A.classes [ B.nav, B.navbarNav ] ]
        ( zipWith li (0 .. length state.dropdowns) state.dropdowns )
      ] ]
  where
  notebookHref :: State -> String
  notebookHref state =
    let u = path2str $ parent state.path in
    updatePath u Config.homeHash

body :: forall p e. State -> [HTML p e]
body state =
  [ if not state.loaded
    then H.h1 [ A.classes [ B.textCenter ] ] [ H.text "Loading..." ]
    else if state.error /= ""
         then H.div [ A.classes [ B.alert, B.alertDanger ] ]
              [ H.h1 [ A.classes [ B.textCenter ] ] [ H.text state.error ] ]
         else contentFluid
              [ H.div [ A.class_ B.clearfix ]
                (cells state <> newCellMenu state)] ]

cells :: forall p e. State -> [HTML p e]
cells state = [ H.div [ A.classes [ Vc.notebookContent ] ] (state ^. notebook <<< notebookCells >>= cell) ]

cell :: forall p e. Cell -> [HTML p e]
cell (Cell o) =
  [ H.div [ A.classes [ B.container, Vc.notebookCell ] ]
    [ divRow [ H.div [ A.classes [ B.btnGroup, B.pullRight ]
                     ]
                     [ H.button [ A.classes [ B.btn ]
                                , E.onClick (E.input_ (ToggleEditorCell o.cellId))
                                ] [ H.text (if o.hiddenEditor then "Show" else "Hide") ]
                     , H.button [ A.classes [ B.btn ]
                                , E.onClick (E.input_ (TrashCell o.cellId))
                                ] [ H.text "Trash" ]
                     ]
             ]
    , divRow (if o.hiddenEditor
              then [ ]
              else [ H.div [ A.classes [ B.colMdOffset2, B.colMd10, Vc.cellInput ] ] [ H.textarea [ ] [ H.text o.input ] ] ])
    , margined [ H.button [ A.classes [ B.btn ] ] [ H.text "Run" ] ]
               [ H.text (Ap.printJson (Ae.encodeJson o.cellType)) ]
               -- , H.button [ A.classes [ B.btn ] ] [ H.text "Status Details" ]
    , H.div [ A.classes [ B.row, Vc.cellOutput ] ] [ H.text o.output ]
    , H.div [ A.classes [ B.row, Vc.cellNextActions ] ] [ ]
    ] ]

divRow :: forall p e. [HTML p e] -> HTML p e
divRow = H.div [ A.classes [ B.row ] ]

margined :: forall p e. [HTML p e] -> [HTML p e] -> HTML p e
margined l r = divRow [ H.div [ A.classes [ B.colMd2 ] ] l
                      , H.div [ A.classes [ B.colMd10 ] ] r
                      ]
newCellMenu :: forall p e. State -> [HTML p e]
newCellMenu state =
  [ H.a [ A.href "#"
        , A.classes [ B.btn, B.btnLink, B.btnLg, Vc.notebookAddCellButton ]
        , E.onClick (\_ -> E.stopPropagation *>
                           E.preventDefault $>
                           (pure $ SetAddingCell (not state.addingCell))) ]
    [ glyph B.glyphiconPlusSign ]
  , H.div [ A.classes [ B.clearfix ] ] [] 
  , H.div [ E.onClick (\_ -> E.stopPropagation $> empty) 
          , A.classes ([ B.panel
                       , B.panelDefault
                       , B.fade
                       , Vc.notebookAddCellMenu ] <>
                       if state.addingCell
                       then [B.in_]
                       else [])]
    [ H.div [ A.classes [ B.panelBody ] ]
      [ H.ul [ A.classes [ B.listInline ] ]
        [ li QueryInsert B.glyphiconHdd
        , li MarkdownInsert B.glyphiconEdit
        , li SearchInsert B.glyphiconSearch ] ] ] ]
  where
  li :: MenuInsertSignal -> A.ClassName -> HTML p e
  li inp cls =
    H.li_ [ H.a [ A.href "#"
                , E.onClick (\e -> do
                                E.stopPropagation
                                E.preventDefault $> do
                                  handleMenuSignal <<< inj $ inp ) ]
            [ glyph cls ] ]

txt :: forall p e. Int -> String -> [HTML p e]
txt lvl text =
  [ H.text $ (joinWith "" $ replicate (toNumber lvl) "--") <> " " <> text ]


li :: forall p e. Number ->  DropdownItem -> HTML p e
li i {visible: visible, name: name, children: children} =
  H.li [ E.onClick (\ev -> do E.stopPropagation
                              E.input_ (Dropdown i) ev)
       , A.classes $ [ B.dropdown ] <>
         (if visible then [ B.open ] else [ ]) ]
  [ H.a [ A.href "#"
        , E.onClick (\_ -> E.preventDefault $> empty)] (txt (fromNumber 0) name)
  , H.ul [ A.classes [ B.dropdownMenu ] ]
    (menuItem <$> children) ]

menuItem :: forall p e. MenuElement -> HTML p e
menuItem {name: name, message: mbMessage, lvl: lvl} =
  H.li [ A.classes (maybe [B.disabled] (const []) mbMessage) ]
  [ H.a [ A.href "#"
        , E.onClick (\e -> do
                        E.stopPropagation 
                        E.preventDefault $>
                          maybe empty handleMenuSignal mbMessage) ]
    [H.span_ $ (txt lvl name) <>
     (maybe [glyph B.glyphiconChevronRight] (const []) mbMessage) ]]


name :: forall p e. State -> HTML p e
name state =
  H.div [ A.classes [ B.colXs12, B.colSm8 ] ]
  [ H.input [ A.classes [ Vc.notebookName ]
            , A.id_ Config.notebookNameEditorId
            , E.onInput (E.input SetName)
            , E.onKeyUp (\e -> if e.keyCode == 13 then
                                 pure $ handleSubmitName state
                               else pure empty)
            , A.value (state.name)  ] [] ]

modal :: forall p e. State -> [HTML p e]
modal state =
  [ H.div [ A.classes ([B.modal, B.fade] <> if state.modalError /= ""
                                            then [B.in_]
                                            else [])
          , E.onClick (E.input_ $ SetModalError "") ]
    [ H.div [ A.classes [ B.modalDialog ] ]
      [ H.div [ A.classes [ B.modalContent ] ]
        [ Vm.header $ Vm.h4 "Error"
        , Vm.body
          [ H.div [ A.classes [ B.alert, B.alertDanger ] ]
            [ H.text state.modalError ] ] 
        ]
      ]
    ]
  ]
  
