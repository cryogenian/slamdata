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

module SlamData.Workspace.Card.OpenResource.Component
  ( openResourceComponent
  , module SlamData.Workspace.Card.OpenResource.Component.Query
  , module SlamData.Workspace.Card.OpenResource.Component.State
  ) where

import SlamData.Prelude

import Data.Array as Arr
import Data.Foldable as F
import Data.Lens as Lens
import Data.Lens ((?~), (.~))
import Data.Path.Pathy (printPath, peel)

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Effects (Slam)
import SlamData.FileSystem.Resource as R
import SlamData.Quasar.FS as Quasar
import SlamData.Render.Common (glyph)
import SlamData.Render.CSS as Rc
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Common.EvalQuery as Eq
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Component as NC
import SlamData.Workspace.Card.OpenResource.Component.Query (QueryP, Query(..))
import SlamData.Workspace.Card.OpenResource.Component.State (State, initialState, _selected, _browsing, _items, _levelOfDetails)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))


import Utils.Path as PU

type HTML = H.ComponentHTML QueryP
type DSL = H.ComponentDSL State QueryP Slam

openResourceComponent ∷ Maybe R.Resource → H.Component NC.CardStateP NC.CardQueryP Slam
openResourceComponent mres =
  NC.makeCardComponent
    { cardType: CT.OpenResource
    , component: H.lifecycleComponent
        { render
        , eval
        , initializer: Just (H.action (right ∘ Init mres))
        , finalizer: Nothing
        }
    , initialState: initialState { selected = Lens.preview Lens._Right ∘ R.getPath =<< mres  }
    , _State: NC._OpenResourceState
    , _Query: NC.makeQueryPrism NC._OpenResourceQuery
    }

render ∷ State → HTML
render state =
  HH.div_
    [ renderHighLOD state
    , renderLowLOD state
    ]


renderLowLOD ∷ State → HTML
renderLowLOD state =
  HH.div
    [ HP.classes
        $ (B.hidden <$ guard (state.levelOfDetails ≠ Low))
        ⊕ [ HH.className "card-input-minimum-lod" ]
    ]
    [ HH.button
        [ ARIA.label "Expand to browse"
        , HP.title "Expand to browse"
        , HP.disabled true
        ]
        [ glyph B.glyphiconFolderOpen
        , HH.text "Please, expand to browse"
        ]
    ]

renderHighLOD ∷ State → HTML
renderHighLOD state =
  HH.div
    [ HP.classes
         $ [ HH.className "card-input-maximum-lod" ]
         ⊕ (B.hidden <$ guard (state.levelOfDetails ≠ High))
    ]
    [ HH.div [ HP.classes [ Rc.openResourceCardMenu ] ]
      [ HH.button
          ([ HP.class_ Rc.formButton
           ] ⊕ case parentDir of
                Nothing →
                  [ HP.disabled true ]
                Just r →
                  [ HE.onClick (HE.input_ (right ∘ (ResourceSelected r)))
                  , HP.title "Up a directory"
                  , ARIA.label "Up a directory"
                  ]
          )
          [ glyph B.glyphiconChevronUp ]
      , HH.p
          [ ARIA.label $ "Selected resource: " ⊕ selectedLabel ]
          [ HH.text selectedLabel ]
      ]
    , HH.ul_ $ map renderItem state.items
    ]

  where
  selectedLabel ∷ String
  selectedLabel =
    fromMaybe ""
    $ map printPath state.selected
    <|> (pure $ printPath state.browsing)

  parentDir ∷ Maybe R.Resource
  parentDir = R.Directory ∘ fst <$> peel state.browsing

  renderItem ∷ R.Resource → HTML
  renderItem r =
    HH.li
      [ HP.classes
          $ ((guard (Just (R.getPath r) ≡ (Right <$> state.selected))) $> B.active)
          ⊕ ((guard (R.hiddenTopLevel r)) $> Rc.itemHidden)
      , HE.onClick (HE.input_ (right ∘ ResourceSelected r))
      , ARIA.label labelTitle

      ]
      [ HH.a
          [ HP.title labelTitle ]
          [ glyphForResource r
          , HH.text $ R.resourceName r
          ]
      ]
    where
    labelTitle ∷ String
    labelTitle =
      "Select " ⊕ R.resourcePath r

glyphForResource ∷ R.Resource → HTML
glyphForResource (R.File _) = glyph B.glyphiconFile
glyphForResource (R.Workspace _) = glyph B.glyphiconBook
glyphForResource (R.Directory _) = glyph B.glyphiconFolderOpen
glyphForResource (R.Mount (R.Database _)) = glyph B.glyphiconHdd
glyphForResource (R.Mount (R.View _)) = glyph B.glyphiconFile


eval ∷ QueryP ~> DSL
eval = cardEval ⨁ openResourceEval

cardEval ∷ Eq.CardEvalQuery ~> DSL
cardEval (Eq.EvalCard info output next) = pure next
cardEval (Eq.Save k) = do
  mbRes ← H.gets _.selected
  k ∘ Card.OpenResource <$>
    case mbRes of
      Just res → pure ∘ Just $ R.File res
      Nothing → do
        br ← H.gets _.browsing
        pure ∘ Just $ R.Directory br
cardEval (Eq.Load card next) = do
  case card of
    Card.OpenResource (Just (res @ R.File _)) → resourceSelected res
    _ → pure unit
  pure next
cardEval (Eq.SetDimensions dims next) = do
  H.modify
    $ (_levelOfDetails
       .~ if dims.width < 288.0 ∨ dims.height < 240.0
            then Low
            else High)
  pure next

openResourceEval ∷ Query ~> DSL
openResourceEval (ResourceSelected r next) = do
  resourceSelected r
  pure next
openResourceEval (Init mres next) = do
  updateItems *> rearrangeItems
  traverse_ resourceSelected mres
  pure next

resourceSelected ∷ R.Resource → DSL Unit
resourceSelected r = do
  case R.getPath r of
    Right fp → do
      for_ (fst <$> peel fp) \dp → do
        oldBrowsing ← H.gets _.browsing
        unless (oldBrowsing ≡ dp) do
          H.modify (_browsing .~ dp)
          updateItems
      H.modify (_selected ?~ fp)
    Left dp → do
      H.modify
        $ (_browsing .~ dp)
        ∘ (_selected .~ Nothing)
      updateItems
  rearrangeItems

updateItems ∷ DSL Unit
updateItems = do
  dp ← H.gets _.browsing
  cs ← Quasar.children dp
  mbSel ← H.gets _.selected
  H.modify (_items .~ foldMap id cs)

rearrangeItems ∷ DSL Unit
rearrangeItems = do
  is ← H.gets _.items
  mbSel ← H.gets $ _.selected >=> findRes is
  let
    withoutSelected =
      Arr.filter (\x → Just x ≠ mbSel) is
    itemsToSet =
      foldMap pure mbSel
      ⊕ Arr.sortBy sortFn withoutSelected
  H.modify (_items .~ itemsToSet)

  where
  findRes ∷ Array R.Resource → PU.FilePath → Maybe R.Resource
  findRes rs path =
    let rpath = Right path
    in F.find (\r → R.getPath r ≡ rpath) rs

  sortFn ∷ R.Resource → R.Resource → Ordering
  sortFn a b | R.hiddenTopLevel a && R.hiddenTopLevel b = compare a b
  sortFn a b | R.hiddenTopLevel a = GT
  sortFn a b | R.hiddenTopLevel b = LT
  sortFn a b = compare a b
