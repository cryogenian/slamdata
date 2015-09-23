{-
Copyright 2015 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module View.Notebook (notebookView) where

import Prelude
import Control.Apply ((*>))
import Control.Bind ((=<<))
import Data.Functor (($>))
import Data.BrowserFeatures (BrowserFeatures())
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Plus (empty)
import Controller.Notebook (I(), handleMenuSignal, handleSubmitName, handleNameInput)
import Controller.Notebook.Cell (runCell)
import Data.Array (range, length, zipWith, replicate, sort, singleton)
import Data.Bifunctor (bimap)
import Data.Either (either)
import Data.Inject1 (inj)
import Data.Int (toNumber, fromNumber)
import Data.KeyCombo (printKeyComboWin, printKeyComboMac, printKeyComboLinux)
import Data.Maybe (Maybe(..), maybe, fromMaybe, isJust)
import Data.Path.Pathy
import Data.Platform (Platform(..))
import Data.String (joinWith)
import Data.These (these)
import Input.Notebook (Input(..))
import Model.Notebook
import Model.Notebook.Cell
import Model.Notebook.Cell.FileInput (_file)
import Model.Notebook.Domain (_cells, _name, _path, notebookPath)
import Model.Notebook.Menu (DropdownItem(), MenuElement(), MenuInsertSignal(..))
import Model.Path ((<./>), encodeURIPath)
import Model.Resource (Resource(), resourcePath)
import Optic.Core
import Optic.Fold ((^?))
import Optic.Index (ix)
import View.Common
import View.Notebook.Cell (cell)
import View.Notebook.Cell.Search (searchOutput)
import View.Notebook.Common (HTML())
import View.Notebook.Modal (modal)

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Forms as E
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.Themes.Bootstrap3 as B
import qualified View.Css as Vc
import qualified View.Modal.Common as Vm

notebookView :: forall e. State -> HTML e
notebookView state =
  H.div [ A.classes classes, E.onClick (E.input_ CloseDropdowns) ]
  (navigation state <> [body state] <> [modal state])
  where classes = if not state.editable
                  then [ Vc.notebookViewHack ]
                  else [ ]

navigation :: forall e. State -> Array (HTML e)
navigation state =
  if (not state.loaded) || (not state.editable) || (isJust state.error)
  then [ ]
  else [ navbar [ H.div [ A.classes [Vc.header, B.clearfix] ]
                        [ icon B.glyphiconBook notebookHref "Notebook"
                        , logo (state ^. _version)
                        , notebookName state
                        ]
                   , H.ul [ A.classes [Vc.headerMenu] ]
                          $ zipWith (dropdownListItem state) (range 0 (length state.dropdowns)) state.dropdowns
                   ]
          ]
  where
  notebookHref = these exploreHref editHref (\_ -> editHref) (state ^. _notebook .. _name)

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
  else
  case state.error of
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


cells :: forall e. State -> Array (HTML e)
cells state = [ H.div [ A.classes [ Vc.notebookContent ] ]
                      $ (state ^. _notebook .. _cells) >>= cell state
              ]

newCellMenu :: forall e. State -> Array (HTML e)
newCellMenu state =
  [ H.ul [ A.classes [ Vc.newCellMenu ] ] $
    [ H.li_ [ H.button [ A.classes [ B.btnLg, B.btnLink ]
                       , E.onClick (E.input_ $ WithState (_addingCell %~ not))
                       , A.title "Insert new cell"
                       ]

              [ glyph $ if state ^. _addingCell
                        then B.glyphiconMinus
                        else B.glyphiconPlus
              ]

            ]
    ] <> listElements
   ]
  where
  listElements :: Array (HTML e)
  listElements =
    [ insertMenuItem "Query" QueryInsert B.glyphiconHdd
    , insertMenuItem "Markdown" MarkdownInsert B.glyphiconEdit
    , insertMenuItem "Explore" ExploreInsert B.glyphiconEyeOpen
    , insertMenuItem "Search" SearchInsert B.glyphiconSearch
    ]


  insertMenuItem :: String -> MenuInsertSignal -> A.ClassName -> HTML e
  insertMenuItem title inp cls =
    H.li_
    [ H.button [ A.title title
               , E.onClick (\_ -> pure <<< handleMenuSignal state <<< inj $ inp)
               , A.classes (fadeWhen $ not (state ^. _addingCell))
               ]

      [ glyph cls ]
    ]


txt :: forall e. Int -> String -> Array (HTML e)
txt lvl text =
  [ H.text $ (joinWith "" $ replicate lvl "--") <> " " <> text ]

dropdownListItem :: forall e. State -> Int ->  DropdownItem -> HTML e
dropdownListItem state i {visible: visible, name: name, children: children} =
  H.li [ E.onClick (\ev -> do E.stopPropagation
                              E.input_ (Dropdown i) ev)
       , A.classes $ [ B.dropdown ] <>
         (if visible then [ B.open ] else [ ]) ]
  [ H.a [ A.href "#"
        , E.onClick (\_ -> E.preventDefault $> empty)] (txt 0 name)
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

notebookName :: forall e. State -> HTML e
notebookName state =
  H.div [ A.classes [Vc.notebookName] ]
        [ H.input [ A.id_ Config.notebookNameEditorId
                  , E.onInput (pure <<< handleNameInput)
                  , E.onChange (\ev -> pure $ handleSubmitName state ev.target)
                  , A.value (these id id (\n _ -> n) $ state ^. _notebook .. _name)
                  ]
                  []
        ]
