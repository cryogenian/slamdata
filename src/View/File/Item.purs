module View.File.Item (items) where

import Data.Inject1 (inj)
import Controller.File.Item (handleOpenItem, handleMoveItem, handleDeleteItem, handleShare, handleConfigure)
import Data.Array ((..), length, zipWith)
import Data.Monoid (mempty)
import Data.Tuple (Tuple(..))
import Input.File.Item (ItemInput(..))
import Model.File (State())
import Model.File.Item (Item(), up)
import Model.Path (decodeURIPath)
import Model.File.Resource (Resource(..))
import Utils.Halide (targetLink')
import View.File.Common (I(), toolItem)
import qualified Data.StrMap as SM
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.Themes.Bootstrap3 as B
import qualified View.Css as Vc

items :: forall p e. State -> H.HTML p (I e)
items state =
  H.div [ A.classes [B.listGroup, Vc.results] ]
        $ zipWith (item state.searching) (0..length state.items) state.items

item :: forall p e. Boolean -> Number -> Item -> H.HTML p (I e)
item searching ix state =
  H.div [ A.classes ([B.listGroupItem] ++ if state.selected then [B.listGroupItemInfo] else mempty)
        , E.onMouseOver (E.input_ $ inj $ ItemHover ix true)
        , E.onClick (E.input_ $ inj $ ItemSelect ix true)
        , E.onDoubleClick (\_ -> pure $ handleOpenItem state)
        ]
        [ H.div [ A.class_ B.row ]
                [ H.div [ A.classes [B.colSm9, Vc.itemContent] ]
                        [ H.a (targetLink' $ handleOpenItem state)
                              [ H.span_ [ H.i [ iconClasses state ]
                                              []
                                        , H.text $ (if searching then decodeURIPath state.root else "") <> state.name
                                        ]
                              ]
                        ]
                , H.div [ A.classes [B.colSm3, Vc.itemToolbar] ]
                        [ H.ul [ A.classes ([B.listInline, B.pullRight] ++ if not $ state.hovered || state.selected then [B.hidden] else mempty)
                               , A.style $ A.styles $ SM.fromList [Tuple "margin-bottom" "0"]
                               ]
                               (showToolbar state)
                        ]
                ]
        ]

iconClasses :: forall e. Item -> A.Attr (I e)
iconClasses item = A.classes [B.glyphicon, Vc.itemIcon, iconClass item.resource]
  where
  iconClass :: Resource -> A.ClassName
  iconClass File      = B.glyphiconFile
  iconClass Database  = B.glyphiconHdd
  iconClass Notebook  = B.glyphiconBook
  iconClass Directory = B.glyphiconFolderOpen
  iconClass Table     = B.glyphiconTh

showToolbar :: forall p e. Item -> [H.HTML p (I e)]
showToolbar item | item.name == up = []
                 | otherwise =
  let conf = case item.resource of
        Database -> [toolItem' handleConfigure "configure" B.glyphiconWrench]
        _ -> []
  in conf <> [ toolItem' handleMoveItem "move/rename" B.glyphiconMove
             , toolItem' handleDeleteItem "remove" B.glyphiconTrash
             , toolItem' handleShare "share" B.glyphiconShare
             ]
  where
  toolItem' :: forall p e. (Item -> I e) -> String -> A.ClassName -> H.HTML p (I e)
  toolItem' f = toolItem [] item f
