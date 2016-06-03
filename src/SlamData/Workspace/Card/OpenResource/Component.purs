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

import Control.Monad.Eff.Exception as Exn
import Control.Monad.Error.Class as Err

import Data.Argonaut (decodeJson, encodeJson)
import Data.Array as Arr
import Data.Foldable as F
import Data.Lens ((?~), (.~))
import Data.Path.Pathy (printPath, peel)

import Halogen as H
import Halogen.Component.Utils as HU
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
import SlamData.Workspace.Card.Common.EvalQuery (liftWithCanceler')
import SlamData.Workspace.Card.Common.EvalQuery as Eq
import SlamData.Workspace.Card.Component as NC
import SlamData.Workspace.Card.OpenResource.Component.Query (QueryP, Query(..))
import SlamData.Workspace.Card.OpenResource.Component.State (State, initialState, _selected, _browsing, _items, _loading)
import SlamData.Workspace.Card.Port as Port

import Utils.Path as PU

type ORHTML = H.ComponentHTML QueryP
type ORDSL = H.ComponentDSL State QueryP Slam

openResourceComponent ∷ H.Component NC.CardStateP NC.CardQueryP Slam
openResourceComponent =
  NC.makeCardComponent
    { cardType: CT.OpenResource
    , component: H.lifecycleComponent
        { render
        , eval
        , initializer: Just (H.action (right ∘ Init))
        , finalizer: Nothing
        }
    , initialState: initialState
    , _State: NC._OpenResourceState
    , _Query: NC.makeQueryPrism NC._OpenResourceQuery
    }

render ∷ State → ORHTML
render state =
  HH.div
    [ HP.classes (Rc.loading <$ guard state.loading) ]
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
    , HH.ul_ (map renderItem state.items)
    ]

  where
  selectedLabel ∷ String
  selectedLabel =
    fromMaybe ""
    $ map printPath state.selected
    <|> (pure $ printPath state.browsing)

  parentDir ∷ Maybe R.Resource
  parentDir = R.Directory ∘ fst <$> peel state.browsing

  renderItem ∷ R.Resource → ORHTML
  renderItem r =
    HH.li
      [ HP.classes
          $ ((guard (Just (R.getPath r) ≡ (Right <$> state.selected))) $> B.active)
          ⊕ ((guard (R.hiddenTopLevel r)) $> Rc.itemHidden)
      , HE.onClick (HE.input_ (right ∘ ResourceSelected r))
      , ARIA.label $ "Select " ⊕ R.resourcePath r
      ]
      [ HH.a_
          [ glyphForResource r
          , HH.text $ R.resourceName r
          ]
      ]
  glyphForResource ∷ R.Resource → ORHTML
  glyphForResource (R.File _) = glyph B.glyphiconFile
  glyphForResource (R.Workspace _) = glyph B.glyphiconBook
  glyphForResource (R.Directory _) = glyph B.glyphiconFolderOpen
  glyphForResource (R.Mount (R.Database _)) = glyph B.glyphiconHdd
  glyphForResource (R.Mount (R.View _)) = glyph B.glyphiconFile

eval ∷ QueryP ~> ORDSL
eval = coproduct cardEval openResourceEval

cardEval ∷ Eq.CardEvalQuery ~> ORDSL
cardEval (Eq.EvalCard info k) =
  k <$> Eq.runCardEvalT do
    mbRes ← lift $ H.gets _.selected
    case mbRes of
      Nothing → pure Nothing
      Just resource → do
        msg ←
          Quasar.messageIfFileNotFound
            resource
            ("File " ⊕ printPath resource ⊕ " doesn't exist")
          # liftWithCanceler'
          # lift
        case msg of
          Right Nothing →
            pure ∘ Just $ Port.TaggedResource { resource, tag: Nothing }
          Right (Just err) →
            Err.throwError err
          Left exn →
            Err.throwError $ Exn.message exn
cardEval (Eq.NotifyRunCard next) = pure next
cardEval (Eq.Save k) = do
  mbRes ← H.gets _.selected
  k <$> case mbRes of
    Just res → pure $ encodeJson $ printPath <$> mbRes
    Nothing → do
      br ← H.gets _.browsing
      pure $ encodeJson $ R.Directory br
cardEval (Eq.Load js next) = do
  for_ (decodeJson js) \path ->
    for_ (PU.parseFilePath path) \fp ->
      resourceSelected (R.File fp)
  pure next
cardEval (Eq.SetupCard info next) = pure next
cardEval (Eq.SetCanceler _ next) = pure next
cardEval (Eq.SetDimensions _ cont) = pure $ cont true
cardEval (Eq.NotifyStopCard next) = pure next


openResourceEval ∷ Query ~> ORDSL
openResourceEval (ResourceSelected r next) = do
  loading ← H.gets _.loading
  when loading do
    HU.sendAfter zero (left $ Eq.NotifyStopCard unit)
    H.modify (_loading .~ false)
  resourceSelected r
  pure next
openResourceEval (Init next) = do
  updateItems *> rearrangeItems $> next

resourceSelected ∷ R.Resource → ORDSL Unit
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

updateItems ∷ ORDSL Unit
updateItems = do
  dp ← H.gets _.browsing
  H.modify (_loading .~ true)
  cs ←
    Quasar.children dp
      # liftWithCanceler'
  mbSel ← H.gets _.selected
  H.modify (_items .~ either (const []) id cs)
  H.modify (_loading .~ false)

rearrangeItems ∷ ORDSL Unit
rearrangeItems = do
  is ← H.gets _.items
  mbSel ← (\ms → ms >>= findRes is) <$> H.gets _.selected
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
    in F.find (\r → R.getPath r == rpath) rs
  sortFn ∷ R.Resource → R.Resource → Ordering
  sortFn a b | R.hiddenTopLevel a && R.hiddenTopLevel b = compare a b
  sortFn a b | R.hiddenTopLevel a = GT
  sortFn a b | R.hiddenTopLevel b = LT
  sortFn a b = compare a b
