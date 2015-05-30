module View.Notebook (view) where

import Api.Fs (saveNotebook)
import Control.Apply ((*>))
import Control.Functor (($>))
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Bind ((=<<))
import Control.Plus (empty)
import Controller.Notebook (I(), handleMenuSignal, handleSubmitName, handleNameInput)
import Controller.Notebook.Cell (runCell)
import Data.Array (range, length, zipWith, replicate, sort, singleton)
import Data.Bifunctor (bimap)
import Data.Either (either)
import Data.Inject1 (inj)
import Data.Int (toNumber, fromNumber, Int())
import Data.KeyCombo (printKeyComboWin, printKeyComboMac, printKeyComboLinux)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Path.Pathy
import Data.Platform (Platform(..))
import Data.String (joinWith)
import Data.These (these)
import Driver.File.Path (updatePath)
import Input.Notebook (Input(..))
import Model.Notebook
import Model.Notebook.Cell
import Model.Notebook.Cell.FileInput (_file)
import Model.Notebook.Domain (_cells, _name, _path, notebookPath)
import Model.Notebook.Menu (DropdownItem(), MenuElement(), MenuInsertSignal(..))
import Model.Path ((<./>), encodeURIPath)
import Model.Resource (Resource(), resourcePath)
import Number.Format (toFixed)
import Optic.Core ((..), (^.), (.~))
import Optic.Fold ((^?))
import Optic.Index (ix)
import View.Common (fadeWhen, contentFluid, navbar, icon, logo, glyph, row)
import View.Notebook.Cell (cell)
import View.Notebook.Cell.Search (searchOutput)
import View.Notebook.Common (HTML())

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Forms as E
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.Themes.Bootstrap3 as B
import qualified View.Css as Vc
import qualified View.File.Modal.Common as Vm

view :: forall e. State -> HTML e
view state =
  H.div [ E.onClick (E.input_ CloseDropdowns) ]
  (navigation state <> [body state] <> modal state)

navigation :: forall e. State -> [HTML e]
navigation state =
  let notebookHref = these exploreHref editHref (\_ -> editHref) (state ^. _notebook .. _name)
  in if not state.editable
     then []
     else
       [ navbar
         [ H.div [ A.classes [ Vc.navCont, Vc.notebookNav, B.containerFluid ] ]
           [ icon B.glyphiconBook notebookHref
           , logo
           , name state ]
         , H.ul [ A.classes [ B.nav, B.navbarNav ] ]
           ( zipWith (li state) (range 0 (length state.dropdowns)) state.dropdowns )
         ] ]
  where
  exploreHref :: String -> String
  exploreHref _ =
    let inputRes = either (const Nothing) Just =<< state ^? _notebook .. _cells .. ix 0 .. _content .. _FileInput .. _file
    in maybe "" (\r -> Config.notebookUrl ++ "#/explore" ++ encodeURIPath (resourcePath r)) inputRes
  editHref :: String -> String
  editHref name =
    case notebookPath (state ^. _notebook) of
      Just path -> Config.notebookUrl ++ "#" ++ encodeURIPath (printPath path) ++ "edit"
      Nothing -> ""

body :: forall e. State -> HTML e
body state =
  if not state.loaded
  then H.h1 [ A.classes [ B.textCenter ] ]
            [ H.text "Loading..." ]
  else case state.error of
    Just err ->
      H.div [ A.classes [ B.alert, B.alertDanger ] ]
            [ H.h1 [ A.classes [ B.textCenter ] ]
                   [ H.text err ]
            ]
    Nothing ->
      contentFluid [ H.div [ A.class_ B.clearfix ]
                           $ cells state ++ if state.editable
                                            then newCellMenu state
                                            else []
                   ]


cells :: forall e. State -> [HTML e]
cells state = ((saveOnCellUpdate state) <$>) <$> [ H.div [ A.classes [ Vc.notebookContent ] ]
                ((state ^. _notebook .. _cells) >>= cell state) ]

saveOnCellUpdate :: forall e. State -> I e -> I e
saveOnCellUpdate state e = e
  -- `E.andThen` \_ -> do
  -- liftAff (saveNotebook (state ^. _notebook))
  -- empty

margined :: forall e. [HTML e] -> [HTML e] -> HTML e
margined l r = row [ H.div [ A.classes [ B.colMd2 ] ] l
                      , H.div [ A.classes [ B.colMd10 ] ] r
                      ]

newCellMenu :: forall e. State -> [HTML e]
newCellMenu state =
  [ H.a [ A.href "#"
        , A.classes [ B.btn, B.btnLink, B.btnLg, Vc.notebookAddCellButton ]
        , E.onClick (\_ -> E.stopPropagation *>
                           E.preventDefault $>
                           (pure $ WithState (_addingCell .~ not state.addingCell))) ]
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
  li :: MenuInsertSignal -> A.ClassName -> HTML e
  li inp cls =
    H.li_ [ H.a [ A.href "#"
                , E.onClick (\e -> do
                                E.stopPropagation
                                E.preventDefault $> do
                                  (handleMenuSignal state) <<< inj $ inp ) ]
            [ glyph cls ] ]

txt :: forall e. Int -> String -> [HTML e]
txt lvl text =
  [ H.text $ (joinWith "" $ replicate (toNumber lvl) "--") <> " " <> text ]

li :: forall e. State -> Number ->  DropdownItem -> HTML e
li state i {visible: visible, name: name, children: children} =
  H.li [ E.onClick (\ev -> do E.stopPropagation
                              E.input_ (Dropdown i) ev)
       , A.classes $ [ B.dropdown ] <>
         (if visible then [ B.open ] else [ ]) ]
  [ H.a [ A.href "#"
        , E.onClick (\_ -> E.preventDefault $> empty)] (txt (fromNumber 0) name)
  , H.ul [ A.classes [ B.dropdownMenu ] ]
    (menuItem state <$> children) ]

menuItem :: forall e. State -> MenuElement -> HTML e
menuItem state {name: name, message: mbMessage, lvl: lvl, shortcut: shortcut} =
  H.li [ A.classes (maybe [B.disabled] (const []) mbMessage) ]
  [ H.a [ A.href "#"
        , A.class_ B.clearfix
        , E.onClick (\e -> do
                        E.stopPropagation
                        E.preventDefault $>
                          maybe empty (handleMenuSignal state) mbMessage) ]
    ([ H.span [A.class_ B.pullLeft] $ (txt lvl name) <>
                 (maybe [glyph B.glyphiconChevronRight] (const []) mbMessage)
     ] ++ maybe [] (\s -> [H.span [A.class_ B.pullRight] [ H.text $ printKeyCombo s ]]) shortcut)
  ]
  where
  printKeyCombo | state ^. _platform == Win = printKeyComboWin
                | state ^. _platform == Mac = printKeyComboMac
                | otherwise = printKeyComboLinux

name :: forall e. State -> HTML e
name state =
  H.div [ A.classes [ B.colXs12, B.colSm8 ] ]
        [ H.form [ E.onSubmit (\_ -> E.preventDefault $> handleSubmitName state) ]
                 [ H.input [ A.class_ Vc.notebookName
                           , A.id_ Config.notebookNameEditorId
                           , E.onInput (pure <<< handleNameInput)
                           , A.value (these id id (\n _ -> n) $ state ^. _notebook .. _name)
                           ]
                           []
                 ]
        ]

modal :: forall e. State -> [HTML e]
modal state =
  [ H.div [ A.classes ([B.modal] ++ fadeWhen (state.modalError == ""))
          , E.onClick (E.input_ (WithState (_modalError .~ ""))) ]
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
