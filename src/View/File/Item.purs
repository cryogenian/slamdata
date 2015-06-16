module View.File.Item (items) where

import Controller.File.Item
import Css.Geometry
import Css.Size (px)
import Css.String
import Data.Array (range, length, zipWith)
import Data.Inject1 (inj)
import Input.File.Item (ItemInput(..))
import Model.Action (Action(..))
import Model.File
import Model.File.Item
import Model.Resource (Resource(..), resourcePath, resourceName, isFile, isDatabase, isNotebook, hiddenTopLevel)
import View.File.Common (HTML(), I(), toolItem)
import Optic.Core ((^.))

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.CSS as CSS
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.Themes.Bootstrap3 as B
import qualified View.Css as Vc

items :: forall e. State -> HTML e
items state =
  let items = state ^. _items
  in H.div [ A.classes [B.listGroup, Vc.results] ]
           $ zipWith (item state) (0 `range` length items) items

item :: forall e. State -> Number -> Item -> HTML e
item state ix item =
  case item of
    PhantomItem _ ->
      H.div [ A.classes [B.listGroupItem, Vc.phantom] ]
            [ H.div [ A.class_ B.row ]
                    [ H.div [ A.classes [B.colXs9, Vc.itemContent]]
                            [ H.span_ [ H.img [ A.src "img/spin.svg" ] []
                                      , H.text $ itemName item
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
                       (if hiddenTopLevel (itemResource item)
                        then if (state ^. _showHiddenFiles)
                             then [Vc.itemHidden]
                             else [B.hidden]
                        else []))
          , E.onClick (E.input_ $ inj $ ItemSelect ix)
          , E.onDoubleClick (\_ -> pure $ openItem item (state ^. _sort) (state ^. _salt))
          ]
          [ H.div [ A.class_ B.row ]
                  [ H.div [ A.classes [B.colXs9, Vc.itemContent] ]
                          [ H.a [ A.href $ itemURL (state ^. _sort) (state ^. _salt) Edit item ]
                                [ H.span_ [ H.i [ iconClasses item ] []
                                          , H.text $ itemName item
                                          ]
                                ]
                          ]
                  , H.div [ A.classes $ [B.colXs3, Vc.itemToolbar] ++ if selected
                                                                      then [Vc.selected]
                                                                      else []
                          ]
                          [ H.ul [ A.classes ([B.listInline, B.pullRight])
                                 , CSS.style (marginBottom $ px 0)
                                 ]
                                 $ showToolbar item state
                          ]
                  ]
          ]
  itemName :: Item -> String
  itemName | isSearching state = resourcePath <<< itemResource
           | otherwise = resourceName <<< itemResource

iconClasses :: forall e. Item -> A.Attr (I e)
iconClasses item = A.classes [B.glyphicon, Vc.itemIcon, iconClass $ itemResource item]
  where
  iconClass :: Resource -> A.ClassName
  iconClass (File _) = B.glyphiconFile
  iconClass (Notebook _) = B.glyphiconBook
  iconClass (Directory _) = B.glyphiconFolderOpen
  iconClass (Database _) = B.glyphiconHdd

showToolbar :: forall e. Item -> State -> [HTML e]
showToolbar item state =
  let r = itemResource item
      conf = if isDatabase r
             then [toolItem' (handleConfigure <<< itemResource) "configure" B.glyphiconWrench]
             else []
  in conf <> [ toolItem' handleMoveItem "move/rename" B.glyphiconMove
             , toolItem' handleDeleteItem "remove" B.glyphiconTrash
             ] ++ if isFile r || isNotebook r
                  then [toolItem' (handleShare (state ^. _sort) (state ^. _salt)) "share" B.glyphiconShare]
                  else []
  where
  toolItem' :: forall e. (Item -> I e) -> String -> A.ClassName -> HTML e
  toolItem' f = toolItem [] item f
