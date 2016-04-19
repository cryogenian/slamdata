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

import SlamData.Prelude

import Data.Lens (LensP, lens, (%~), (.~))

import Halogen as H
import Halogen.HTML.CSS.Indexed as CSS
import Halogen.HTML.Events.Handler as HEH
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import CSS.Geometry (marginBottom)
import CSS.Size (px)

import SlamData.Effects (Slam)
import SlamData.FileSystem.Listing.Item (Item(..), itemResource)
import SlamData.FileSystem.Resource (Resource(..), Mount(..), resourceName, resourcePath, isMount, isFile, isNotebook, isViewMount, hiddenTopLevel, root)
import SlamData.Render.CSS as Rc

type State =
  { item ∷ Item
  , isSearching ∷ Boolean
  , isHidden ∷ Boolean
  }

initialState ∷ State
initialState =
  { item: PhantomItem root
  , isSearching: false
  , isHidden: false
  }

_item ∷ LensP State Item
_item = lens _.item _{item = _}

_isSearching ∷ LensP State Boolean
_isSearching = lens _.isSearching _{isSearching = _}

_isHidden ∷ LensP State Boolean
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
  | SharePermissions Resource a

type HTML = H.ComponentHTML Query
type DSL = H.ComponentDSL State Query Slam

comp ∷ H.Component State Query Slam
comp = H.component { render, eval }

render ∷ State → HTML
render state = case state.item of
  SelectedItem _ → itemView state true true
  ActionsPresentedItem _ → itemView state false true
  Item _ → itemView state false false
  PhantomItem _ →
    HH.div
      [ HP.classes [ B.listGroupItem, Rc.phantom ] ]
      [ HH.div
          [ HP.class_ B.row ]
          [ HH.div
              [ HP.classes [ B.colXs8, Rc.itemContent ] ]
              [ HH.span_
                  [ HH.img [ HP.src "img/spin.gif" ]
                  , HH.text $ itemName state
                  ]
              ]
          ]
      ]

eval ∷ Natural Query DSL
eval (Toggle next) = H.modify (_item %~ toggle) $> next
  where
  toggle (Item r) = SelectedItem r
  toggle (ActionsPresentedItem r) = SelectedItem r
  toggle (SelectedItem r) = Item r
  toggle it = it
eval (Deselect next) = H.modify (_item %~ deselect) $> next
  where
  deselect (SelectedItem r) = Item r
  deselect it = it
eval (PresentActions next) = H.modify (_item %~ presentActions) $> next
  where
  presentActions (Item r) = ActionsPresentedItem r
  presentActions it = it
eval (HideActions next) = H.modify (_item %~ hideActions) $> next
  where
  hideActions (ActionsPresentedItem r) = Item r
  hideActions it = it
eval (Open _ next) = pure next
eval (Configure _ next) = pure next
eval (Move _ next) = pure next
eval (Download _ next) = pure next
eval (Remove _ next) = pure next
eval (Share _ next) = pure next
eval (SetIsSearching bool next) = H.modify (_isSearching .~ bool) $> next
eval (SetIsHidden bool next) = H.modify (_isHidden .~ bool) $> next
eval (SharePermissions _ next) = pure next

itemName ∷ State → String
itemName { isSearching, item } =
  let toName = if isSearching then resourcePath else resourceName
  in toName $ itemResource item

itemIsHidden ∷ Item → Boolean
itemIsHidden = hiddenTopLevel <<< itemResource

presentHiddenItem ∷ State → Boolean
presentHiddenItem = not <<< _.isHidden

presentItem ∷ State → Item → Boolean
presentItem state item = (isHidden && presentHiddenItem state) || not isHidden
  where
  isHidden = itemIsHidden item

itemView ∷ State → Boolean → Boolean → HTML
itemView state@{ item } selected presentActions | not (presentItem state item) =
  HH.text ""
itemView state@{ item } selected presentActions | otherwise =
  HH.div
    [ HP.classes itemClasses
    , HE.onClick (HE.input_ Toggle)
    , HE.onMouseEnter (HE.input_ PresentActions)
    , HE.onMouseLeave (HE.input_ HideActions)
    , HE.onDoubleClick $ HE.input_ $ Open (itemResource item)
    , ARIA.label label
    ]
    [ HH.div
        [ HP.class_ B.row ]
        [ HH.div
            [ HP.classes [ B.colXs8, Rc.itemContent ] ]
            [ HH.a
                [ HE.onClick (\_ → HEH.preventDefault
                                    $> H.action (Open (itemResource item))) ]
                [ HH.i [ iconClasses item ] []
                , HH.text $ itemName state
                ]
            ]
        , HH.div
            [ HP.classes $ [ B.colXs4, Rc.itemToolbar ] ⊕ (guard selected $> Rc.selected) ]
            [ itemActions presentActions item ]
        ]
    ]
  where
  itemClasses ∷ Array HH.ClassName
  itemClasses =
    [ B.listGroupItem ]
    ⊕ (guard selected $> B.listGroupItemInfo)
    ⊕ (if itemIsHidden item && presentHiddenItem state then [ Rc.itemHidden ] else [ ])

  label ∷ String
  label | selected  = "Deselect " ++ itemName state
  label | otherwise = "Select " ++ itemName state

iconClasses ∷ forall r i. Item → HP.IProp (class ∷ HP.I | r) i
iconClasses item = HP.classes
  [ B.glyphicon
  , Rc.itemIcon
  , iconClass (itemResource item)
  ]
  where
  iconClass ∷ Resource → HH.ClassName
  iconClass (File _) = B.glyphiconFile
  iconClass (Notebook _) = B.glyphiconBook
  iconClass (Directory _) = B.glyphiconFolderOpen
  iconClass (Mount (Database _)) = B.glyphiconHdd
  iconClass (Mount (View _)) = B.glyphiconFile

itemActions ∷ Boolean → Item → HTML
itemActions presentActions item | not presentActions = HH.text ""
itemActions presentActions item | otherwise =
  HH.ul
    [ HP.classes [ B.listInline, B.pullRight ]
    , CSS.style $ marginBottom (px zero)
    ]
    (conf ⊕ common ⊕ share)
  where
  r ∷ Resource
  r = itemResource item

  conf ∷ Array HTML
  conf = guard (isMount r) $>
    itemAction Configure "Configure" B.glyphiconWrench

  common ∷ Array HTML
  common =
    [
-- Commented until backend is ready
--      itemAction SharePermissions "Share permissions" B.glyphiconShareAlt
      itemAction Move "Move / rename" B.glyphiconMove
    , itemAction Download "Download" B.glyphiconCloudDownload
    , itemAction Remove "Remove" B.glyphiconTrash
    ]

  share ∷ Array HTML
  share = guard (isFile r || isNotebook r || isViewMount r) $>
    itemAction Share "Share" B.glyphiconShare

  itemAction ∷ (Resource → H.Action Query) → String → HH.ClassName → HTML
  itemAction act label cls =
    HH.li_
      [ HH.button
          [ HE.onClick $ HE.input_ (act (itemResource item))
          , HP.title label
          , ARIA.label label
          , HP.class_ Rc.fileAction
          ]
          [ HH.i [ HP.classes [ B.glyphicon, cls ] ] [] ]
      ]
