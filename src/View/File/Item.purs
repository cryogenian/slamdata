module View.File.Item (items) where

import Control.Apply ((*>))
import Control.Monad.Eff.Class (liftEff)
import Control.Plus (empty)
import Controller.File.Item
import Css.Geometry
import Css.Size (px)
import Css.String
import Data.Array ((..), length, zipWith)
import Data.Inject1 (inj)
import Data.Path.Pathy
import Data.Tuple (Tuple(..))
import Input.File.Item (ItemInput(..))
import Model.Action
import Model.File (State())
import Model.File.Item
import Model.Path (decodeURIPath)
import Model.Resource
import View.File.Common (I(), toolItem)

import qualified Data.StrMap as SM
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.CSS as CSS
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.Themes.Bootstrap3 as B
import qualified View.Css as Vc

items :: forall e. State -> H.HTML (I e)
items state =
  H.div [ A.classes [B.listGroup, Vc.results] ]
        $ zipWith (item state) (0..length state.items) state.items

item :: forall e. State -> Number -> Item -> H.HTML (I e)
item state ix item =
  H.div [ A.classes ([B.listGroupItem] ++
                     (if item.selected
                      then [B.listGroupItemInfo]
                      else []) ++
                     (if hiddenTopLevel item.resource
                      then if state.showHiddenFiles
                           then [Vc.itemHidden]
                           else [B.hidden]
                      else []))
        , E.onMouseOver (E.input_ $ inj $ ItemHover ix true)
        , E.onClick (E.input_ $ inj $ ItemSelect ix true)
        , E.onDoubleClick (\_ -> pure $ openItem item state.sort state.salt)
        ]
        [ H.div [ A.class_ B.row ]
          [ H.div [ A.classes [B.colXs9, Vc.itemContent]]
            [ H.a
              [ A.href $ itemURL state.sort state.salt Edit item ]
              [ H.span_ [ H.i [ iconClasses item ] []
                        , H.text $ (if state.searching
                                    then resourcePath
                                    else resourceName) item.resource
                        ]
              ]
            ]
          , H.div [ A.classes [B.colXs3, Vc.itemToolbar] ]
            [ H.ul [ A.classes ([B.listInline, B.pullRight] ++
                                if not $ item.hovered || item.selected
                                then [B.hidden]
                                else [])
                   , CSS.style (marginBottom $ px 0)
                   ]
              (showToolbar item state)
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



showToolbar :: forall e. Item -> State -> [H.HTML (I e)]
showToolbar item state =
  let conf = if isDatabase item.resource
             then [toolItem' (handleConfigure <<< _.resource) "configure" B.glyphiconWrench]
             else []

  in conf <> [ toolItem' handleMoveItem "move/rename" B.glyphiconMove
             , toolItem' handleDeleteItem "remove" B.glyphiconTrash
             ] ++ if isFile item.resource || isNotebook item.resource
                  then [toolItem' (handleShare state.sort state.salt) "share" B.glyphiconShare]
                  else []
  where

  toolItem' :: forall e. (Item -> I e) -> String -> A.ClassName -> H.HTML (I e)
  toolItem' f = toolItem [] item f
