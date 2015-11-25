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

module View.File.Item (items) where

import Prelude
import Controller.File.Common (Event())
import Controller.File.Item
import Css.Geometry (marginBottom)
import Css.Size (px)
import Css.String
import Data.Array (range, length, zipWith)
import Data.Inject1 (inj)
import Input.File.Item (ItemInput(..))
import Model.Action (Action(..))
import Model.File
import Model.File.Item
import Model.Resource (Resource(..), resourcePath, resourceName, isFile, isDatabase, isNotebook, hiddenTopLevel)
import View.File.Common (HTML(), toolItem)
import Optic.Core

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Utils.Halide (ariaLabel) as A
import qualified Halogen.HTML.CSS as CSS
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.Themes.Bootstrap3 as B
import qualified View.Css as Vc

items :: forall e. State -> HTML e
items state =
  let xs = state ^. _items
  in H.div [ A.classes [B.listGroup, Vc.results] ]
           $ zipWith (item state) (0 `range` length xs) xs

item :: forall e. State -> Int -> Item -> HTML e
item state ix it =
  case it of
    PhantomItem _ ->
      H.div [ A.classes [B.listGroupItem, Vc.phantom] ]
            [ H.div [ A.class_ B.row ]
                    [ H.div [ A.classes [B.colXs9, Vc.itemContent]]
                            [ H.span_ [ H.img [ A.src "img/spin.gif" ] []
                                      , H.text $ itemName it
                                      ]
                            ]
                    ]
            ]
    SelectedItem _ -> item' true
    Item _ -> item' false
  where
  item' :: Boolean -> HTML e
  item' selected =
    H.div [ A.classes ([B.listGroupItem] ++
                       (if selected
                        then [B.listGroupItemInfo]
                        else []) ++
                       (if hiddenTopLevel (itemResource it)
                        then if (state ^. _showHiddenFiles)
                             then [Vc.itemHidden]
                             else [B.hidden]
                        else []))
          , E.onClick (E.input_ $ inj $ ItemSelect ix)
          , E.onDoubleClick (\_ -> pure $ openItem it (state ^. _sort) (state ^. _salt))
          ]
          [ H.div [ A.class_ B.row ]
                  [ H.div [ A.classes [B.colXs9, Vc.itemContent] ]
                          [ H.a [ A.href $ itemURL (state ^. _sort) (state ^. _salt) Edit it ]
                                [ H.span_ [ H.i [ iconClasses it ] []
                                          , H.text $ itemName it
                                          ]
                                ]
                          ]
                  , H.div [ A.classes $ [B.colXs3, Vc.itemToolbar] ++ if selected
                                                                      then [Vc.selected]
                                                                      else []
                          ]
                          [ H.ul [ A.classes ([B.listInline, B.pullRight])
                                 , CSS.style (marginBottom $ px 0.0)
                                 ]
                                 $ showToolbar it state
                          ]
                  ]
          ]
  itemName :: Item -> String
  itemName | isSearching state = resourcePath <<< itemResource
           | otherwise = resourceName <<< itemResource

iconClasses :: forall i. Item -> A.Attr i
iconClasses it = A.classes [B.glyphicon, Vc.itemIcon, iconClass $ itemResource it]
  where
  iconClass :: Resource -> A.ClassName
  iconClass (File _) = B.glyphiconFile
  iconClass (ViewMount _) = B.glyphiconFile
  iconClass (Notebook _) = B.glyphiconBook
  iconClass (Directory _) = B.glyphiconFolderOpen
  iconClass (Database _) = B.glyphiconHdd

showToolbar :: forall e. Item -> State -> Array (HTML e)
showToolbar it state =
  let r = itemResource it
      conf = if isDatabase r
             then [toolItem' handleConfigureItem "configure" B.glyphiconWrench]
             else []
  in conf <> [ toolItem' handleMoveItem "Move / rename" B.glyphiconMove
             , toolItem' handleDownloadItem "Download" B.glyphiconDownloadAlt
             , toolItem' handleDeleteItem "Remove" B.glyphiconTrash
             ] ++ if isFile r || isNotebook r
                  then [toolItem' (handleShare (state ^. _sort) (state ^. _salt)) "Share" B.glyphiconShare]
                  else []
  where
  toolItem' :: forall e. (Item -> Event e) -> String -> A.ClassName -> HTML e
  toolItem' f = toolItem [] it f
