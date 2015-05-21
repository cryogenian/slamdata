module View.File.Item (items) where

import Data.Inject1 (inj)
import Controller.File.Item (
  handleOpenItem,
  handleMoveItem,
  handleDeleteItem,
  handleShare,
  handleConfigure)
import Data.Array ((..), length, zipWith)
import Data.Monoid (mempty)
import Data.Tuple (Tuple(..))
import Input.File.Item (ItemInput(..))
import Model.File (State())
import Model.File.Item
import Model.Resource
import Model.Path (decodeURIPath)
import Utils.Halide (targetLink')
import View.File.Common (I(), toolItem)
import qualified Data.StrMap as SM
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.Themes.Bootstrap3 as B
import qualified Halogen.HTML.CSS as CSS
import Css.Size (px)
import Css.Geometry
import Css.String
import qualified View.Css as Vc
import Data.Path.Pathy

items :: forall e. State -> H.HTML (I e)
items state =
  H.div [ A.classes [B.listGroup, Vc.results] ]
        $ zipWith (item state.searching) (0..length state.items) state.items

item :: forall e. Boolean -> Number -> Item -> H.HTML (I e)
item searching ix state =
  H.div [ A.classes ([B.listGroupItem] ++ if state.selected
                                          then [B.listGroupItemInfo]
                                          else mempty)
        , E.onMouseOver (E.input_ $ inj $ ItemHover ix true)
        , E.onClick (E.input_ $ inj $ ItemSelect ix true)
        , E.onDoubleClick (\_ -> pure $ handleOpenItem state)
        ]
        [ H.div [ A.class_ B.row ]
          [ H.div [ A.classes [B.colSm9, Vc.itemContent] ]
            [ H.a (targetLink' $ handleOpenItem state)
              [ H.span_ [ H.i [ iconClasses state ] []
                        , H.text $ (if searching
                                    then resourcePath
                                    else resourceName) state.resource
                                        ]
                              ]
                        ]
          , H.div [ A.classes [B.colSm3, Vc.itemToolbar] ]
            [ H.ul [ A.classes ([B.listInline, B.pullRight] ++
                                if not $ state.hovered || state.selected
                                then [B.hidden]
                                else mempty)
                   , CSS.style (marginBottom $ px 0)
                   ]
              (showToolbar state)
            ]
          ]
        ]

iconClasses :: forall e. Item -> A.Attr (I e)
iconClasses item = A.classes [B.glyphicon, Vc.itemIcon, iconClass item.resource]
  where
  iconClass :: Resource -> A.ClassName
  iconClass r =
    if isFile r
    then B.glyphiconFile
    else if isDatabase r
         then B.glyphiconHdd
         else if isNotebook r
              then B.glyphiconBook
              else B.glyphiconFolderOpen



showToolbar :: forall e. Item -> [H.HTML (I e)]
showToolbar item =
  let conf = if isDatabase item.resource
             then [toolItem' handleConfigure "configure" B.glyphiconWrench]
             else []

  in conf <> [ toolItem' handleMoveItem "move/rename" B.glyphiconMove
             , toolItem' handleDeleteItem "remove" B.glyphiconTrash
             , toolItem' handleShare "share" B.glyphiconShare
             ]
  where
 
  toolItem' :: forall e. (Item -> I e) -> String -> A.ClassName -> H.HTML (I e)
  toolItem' f = toolItem [] item f
