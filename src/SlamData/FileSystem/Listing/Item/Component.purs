{-
Copyright 2016 SlamData, Inc.

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

module SlamData.FileSystem.Listing.Item.Component where

import Prelude

import Control.MonadPlus (guard)

import Data.Functor (($>))
import Data.Lens (LensP(), lens, (%~), (.~))

import Halogen
import Halogen.HTML.CSS.Indexed as CSS
import Halogen.HTML.Events.Handler as E
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import CSS.Geometry (marginBottom)
import CSS.Size (px)

import SlamData.Effects (Slam())
import SlamData.FileSystem.Listing.Item (Item(..), itemResource)
import SlamData.FileSystem.Resource (Resource(..), Mount(..), resourceName, resourcePath, isMount, isFile, isNotebook, isViewMount, hiddenTopLevel, root)
import SlamData.Render.CSS as Rc

type State =
  { item :: Item
  , isSearching :: Boolean
  , isHidden :: Boolean
  }

initialState :: State
initialState =
  { item: PhantomItem root
  , isSearching: false
  , isHidden: false
  }

_item :: LensP State Item
_item = lens _.item _{item = _}

_isSearching :: LensP State Boolean
_isSearching = lens _.isSearching _{isSearching = _}

_isHidden :: LensP State Boolean
_isHidden = lens _.isHidden _{isHidden = _}

data Query a
  = Toggle a
  | PresentActions a
  | HideActions a
  | Deselect a
  | Open Resource a
  | Configure Resource a
  | Move Resource a
  | Download Resource a
  | Remove Resource a
  | Share Resource a
  | SetIsSearching Boolean a
  | SetIsHidden Boolean a

comp :: Component State Query Slam
comp = component render eval

render :: State -> ComponentHTML Query
render state = case state.item of
  SelectedItem _ -> itemView state true true
  ActionsPresentedItem _ -> itemView state false true
  Item _ -> itemView state false false
  PhantomItem _ ->
    H.div
      [ P.classes [ B.listGroupItem, Rc.phantom ] ]
      [ H.div
          [ P.class_ B.row ]
          [ H.div
              [ P.classes [ B.colXs9, Rc.itemContent ] ]
              [ H.span_
                  [ H.img [ P.src "img/spin.gif" ]
                  , H.text $ itemName state
                  ]
              ]
          ]
      ]
eval :: Eval Query State Query Slam
eval (Toggle next) = modify (_item %~ toggle) $> next
  where
  toggle (Item r) = SelectedItem r
  toggle (ActionsPresentedItem r) = SelectedItem r
  toggle (SelectedItem r) = Item r
  toggle it = it
eval (Deselect next) = modify (_item %~ deselect) $> next
  where
  deselect (SelectedItem r) = Item r
  deselect it = it
eval (PresentActions next) = modify (_item %~ presentActions) $> next
  where
  presentActions (Item r) = ActionsPresentedItem r
  presentActions it = it
eval (HideActions next) = modify (_item %~ hideActions) $> next
  where
  hideActions (ActionsPresentedItem r) = Item r
  hideActions it = it
eval (Open _ next) = pure next
eval (Configure _ next) = pure next
eval (Move _ next) = pure next
eval (Download _ next) = pure next
eval (Remove _ next) = pure next
eval (Share _ next) = pure next
eval (SetIsSearching bool next) = modify (_isSearching .~ bool) $> next
eval (SetIsHidden bool next) = modify (_isHidden .~ bool) $> next

itemName :: State -> String
itemName { isSearching, item } =
  let toName = if isSearching then resourcePath else resourceName
  in toName $ itemResource item

itemIsHidden :: Item -> Boolean
itemIsHidden = hiddenTopLevel <<< itemResource

presentHiddenItem :: State -> Boolean
presentHiddenItem = not <<< _.isHidden

presentItem :: State -> Item -> Boolean
presentItem state item = (isHidden && presentHiddenItem state) || not isHidden
  where
  isHidden = itemIsHidden item

itemView :: forall p. State -> Boolean -> Boolean -> HTML p Query
itemView state@{ item } selected presentActions | not (presentItem state item) = H.text ""
itemView state@{ item } selected presentActions | otherwise =
  H.div
    [ P.classes itemClasses
    , E.onClick (E.input_ Toggle)
    , E.onMouseEnter (E.input_ PresentActions)
    , E.onMouseLeave (E.input_ HideActions)
    , E.onDoubleClick $ E.input_ $ Open (itemResource item)
    , ARIA.label label
    ]
    [ H.div
        [ P.class_ B.row ]
        [ H.div [ P.classes [ B.colXs9, Rc.itemContent ] ]
            [ H.a
                [ E.onClick (\_ -> E.preventDefault $> action (Open (itemResource item))) ]
                [ H.i [ iconClasses item ] []
                , H.text $ itemName state
                ]
            ]
        , H.div
            [ P.classes $ [ B.colXs3, Rc.itemToolbar ] <> (guard selected $> Rc.selected) ]
            [ itemActions presentActions item ]
        ]
    ]
  where
  itemClasses :: Array H.ClassName
  itemClasses =
    [ B.listGroupItem ]
    <> (guard selected $> B.listGroupItemInfo)
    <> (if itemIsHidden item && presentHiddenItem state then [ Rc.itemHidden ] else [ ])

  label :: String
  label | selected  = "Deselect " ++ itemName state
  label | otherwise = "Select " ++ itemName state

iconClasses :: forall r i. Item -> P.IProp (class :: P.I | r) i
iconClasses item = P.classes
  [ B.glyphicon
  , Rc.itemIcon
  , iconClass (itemResource item)
  ]
  where
  iconClass :: Resource -> H.ClassName
  iconClass (File _) = B.glyphiconFile
  iconClass (Notebook _) = B.glyphiconBook
  iconClass (Directory _) = B.glyphiconFolderOpen
  iconClass (Mount (Database _)) = B.glyphiconHdd
  iconClass (Mount (View _)) = B.glyphiconFile

itemActions :: forall p. Boolean -> Item -> HTML p Query
itemActions presentActions item | not presentActions = H.text ""
itemActions presentActions item | otherwise =
  H.ul
    [ P.classes [ B.listInline, B.pullRight ]
    , CSS.style $ marginBottom (px zero)
    ]
    (conf <> common <> share)
  where
  r :: Resource
  r = itemResource item

  conf :: Array (HTML p Query)
  conf = guard (isMount r) $>
    itemAction Configure "Configure" B.glyphiconWrench

  common :: Array (HTML p Query)
  common =
    [ itemAction Move "Move / rename" B.glyphiconMove
    , itemAction Download "Download" B.glyphiconCloudDownload
    , itemAction Remove "Remove" B.glyphiconTrash
    ]

  share :: Array (HTML p Query)
  share = guard (isFile r || isNotebook r || isViewMount r) $>
    itemAction Share "Share" B.glyphiconShare

  itemAction :: (Resource -> Action Query) -> String -> H.ClassName -> HTML p Query
  itemAction act label cls =
    H.li_
      [ H.button
          [ E.onClick $ E.input_ (act (itemResource item))
          , P.title label
          , ARIA.label label
          , P.class_ Rc.fileAction
          ]
          [ H.i [ P.classes [ B.glyphicon, cls ] ] [] ]
      ]
