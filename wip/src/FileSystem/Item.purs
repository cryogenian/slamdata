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

module FileSystem.Item where

import Prelude

import Control.Monad.Aff (attempt)
import Control.MonadPlus (guard)

import Data.Array (singleton)
import Data.Either (Either(..))
import Data.Functor (($>))
import Data.Lens (LensP(), (^.), lens, (%~), (.~), (?~))
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)

import Halogen.Component (Component(), Eval(), ComponentHTML(), component)
import Halogen.CustomProps as Cp
import Halogen.HTML as H
import Halogen.HTML.Core (HTML(), ClassName(), Prop())
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import Halogen.Query (action, modify, gets, liftAff')
import Halogen.Themes.Bootstrap3 as B

import Css.Geometry (marginBottom)
import Css.Size (px)

import FileSystem.Common (Slam())
import Model.Item (Item(..), itemResource)
import Model.Resource  (Resource(..), resourceName, resourcePath, isDatabase, isFile, isNotebook, isViewMount, hiddenTopLevel, root)
import Quasar.Aff as API
import Render.CssClasses as Rc

type StateRec =
  { item :: Item
  , isSearching :: Boolean
  , isHidden :: Boolean
  , mbURI :: Maybe String
  }

data State = State StateRec

initialState :: State
initialState =
  State { item: PhantomItem root
        , isSearching: false
        , isHidden: false
        , mbURI: Nothing
        }

_State :: LensP State StateRec
_State = lens (\(State obj) -> obj) (const State)

_item :: LensP State Item
_item = _State <<< lens _.item _{item = _}

_isSearching :: LensP State Boolean
_isSearching = _State <<< lens _.isSearching _{isSearching = _}

_isHidden :: LensP State Boolean
_isHidden = _State <<< lens _.isHidden _{isHidden = _}

_mbURI :: LensP State (Maybe String)
_mbURI = _State <<< lens _.mbURI _{mbURI = _}

data Query a
  = Toggle a
  | Deselect a
  | Open a
  | Configure a
  | Move a
  | Download a
  | Remove a
  | Share a
  | GetItem (Item -> a)
  | SetIsSearching Boolean a
  | SetIsHidden Boolean a
  | GetURI (Maybe String -> a)

comp :: Component State Query Slam
comp = component render eval

render :: State -> ComponentHTML Query
render state = case (state ^. _item) of
  SelectedItem _ -> itemView state true
  Item _ -> itemView state false
  PhantomItem _ ->
    H.div [ P.classes [ B.listGroupItem, Rc.phantom ] ]
    [ H.div [ P.class_ B.row ]
      [ H.div [ P.classes [ B.colXs9, Rc.itemContent ] ]
        [ H.span_ [ H.img [ P.src "img/spin.gif" ]
                  , H.text $ itemName state
                  ]
        ]
      ]
    ]
eval :: Eval Query State Query Slam
eval (Toggle next) = do
  modify (_item %~ toggle)
  pure next
  where
  toggle (Item r) = SelectedItem r
  toggle (SelectedItem r) = Item r
  toggle it = it
eval (Deselect next) = do
  modify (_item %~ deselect)
  pure next
  where
  deselect (SelectedItem r) = Item r
  deselect it = it
eval (Open next) = pure next
eval (Configure next) = do
  res <- gets ((^. _item) >>> itemResource)
  eURI <- liftAff' $ attempt $ API.mountInfo res
  case eURI of
    Left _ -> modify (_mbURI .~ Nothing)
    Right uri -> modify (_mbURI ?~ uri)
  pure next
eval (Move next) = pure next
eval (Download next) = pure next
eval (Remove next) =  pure next
eval (Share next) = pure next
eval (GetItem continue) = map continue $ gets (^. _item)
eval (SetIsSearching bool next) = do
  modify (_isSearching .~ bool)
  pure next
eval (SetIsHidden bool next) = do
  modify (_isHidden .~ bool)
  pure next
eval (GetURI continue) = do
  map continue $ gets (^. _mbURI)

itemName :: State -> String
itemName state
  | state ^. _isSearching  = resourcePath $ itemResource $ (state ^. _item)
  | otherwise = resourceName $ itemResource $ (state ^. _item)


itemView :: forall p. State -> Boolean -> HTML p (Query Unit)
itemView state selected =
  H.div [ P.classes itemClasses
        , E.onClick (E.input_ Toggle)
        , E.onDoubleClick (E.input_ Open)
        ]
  [ H.div [ P.class_ B.row ]
    [ H.div [ P.classes [ B.colXs9, Rc.itemContent ] ]
      [ H.a [ E.onClick (\_ -> E.preventDefault
                               $> (action $ Open)
                        )
            ]
        [ H.span_ [ H.i [ iconClasses (state ^. _item) ] [ ]
                  , H.text $ itemName state
                  ]
        ]
      ]
    , H.a [ P.classes $ [ B.colXs3, Rc.itemToolbar ]
            <> (guard selected $> Rc.selected)
          ]
      [ H.ul [ P.classes [ B.listInline, B.pullRight ]
             , CSS.style (marginBottom $ px zero)
             ]
        $ showToolbar (state ^. _item)
      ]
    ]
  ]
  where
  it :: Item
  it = state ^. _item
  itemClasses :: Array ClassName
  itemClasses =
    [ B.listGroupItem ]
    <> (if selected
        then [ B.listGroupItemInfo ]
        else [ ])
    <> (if hiddenTopLevel (itemResource (state ^. _item))
        then if (state ^. _isHidden)
             then [ B.hidden ]
             else [ Rc.itemHidden ]
        else [ ])

iconClasses :: forall i. Item -> Prop i
iconClasses it = P.classes [ B.glyphicon, Rc.itemIcon, iconClass $ itemResource it]
  where
  iconClass :: Resource -> ClassName
  iconClass (File _) = B.glyphiconFile
  iconClass (Notebook _) = B.glyphiconBook
  iconClass (Directory _) = B.glyphiconFolderOpen
  iconClass (Database _) = B.glyphiconHdd
  iconClass (ViewMount _) = B.glyphiconFile


showToolbar :: forall p. Item -> Array (HTML p (Query Unit))
showToolbar it =
  conf <> common <> share
  where
  r :: Resource
  r = itemResource it

  conf :: Array (HTML p (Query Unit))
  conf = if isDatabase r
         then singleton $ toolItem Configure "Configure" B.glyphiconWrench
         else mempty

  common = [ toolItem Move "Move / rename" B.glyphiconMove
           , toolItem Download "Download" B.glyphiconDownloadAlt
           , toolItem Remove "Remove" B.glyphiconTrash
           ]

  share :: Array (HTML p (Query Unit))
  share = if isFile r || isNotebook r || isViewMount r
          then singleton $ toolItem Share "Share" B.glyphiconShare
          else mempty

  toolItem func label cls =
    H.li_ [ H.button [ E.onClick (\_ -> pure $ action func)
                     , Cp.ariaLabel label
                     , P.title label
                     ]
            [ H.i [ P.title label
                  , P.classes [ B.glyphicon, cls ]
                  ]
              [ ]
            ]
          ]
