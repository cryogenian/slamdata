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

module SlamData.Workspace.MillerColumns.Component
  ( ColumnOptions
  , InitialItemState(..)
  , ItemHTML
  , component
  , module SlamData.Workspace.MillerColumns.Component.Query
  , module SlamData.Workspace.MillerColumns.Component.State
  ) where

import SlamData.Prelude

import Control.Monad.Eff (Eff)

import Data.Array as A
import Data.Traversable (scanr)
import Data.List ((:))
import Data.List as L
import Data.Unfoldable (replicate)

import DOM (DOM)
import DOM.HTML.Types (HTMLElement, htmlElementToElement)
import DOM.Node.Element (scrollWidth, setScrollLeft) as DOM
import DOM.HTML.HTMLElement (offsetWidth) as DOM

import Halogen as H
import Halogen.Component.Utils (raise')
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA

import SlamData.Monad (Slam)
import SlamData.Render.Common (clearFieldIcon)
import SlamData.Workspace.MillerColumns.Component.Query (ItemQuery(..), ItemQuery', Query(..), Query')
import SlamData.Workspace.MillerColumns.Component.State (State, State', initialState)

type ColumnOptions a i s f =
  { render
      ∷ L.List i
      → a
      → InitialItemState
      → { component :: H.Component s (ItemQuery i ⨁ f) Slam
         , initialState :: s
         }
  , label ∷ a → String
  , load ∷ L.List i → Slam (Maybe (L.List a))
  , id ∷ a → i
  }

data InitialItemState = Selected | Deselected

derive instance eqInitialItemState :: Eq InitialItemState
derive instance ordInitialItemState :: Ord InitialItemState

type HTML i s f = H.ParentHTML s (Query i) (ItemQuery' i f) Slam (L.List i)
type DSL a i s f = H.ParentDSL (State a i) s (Query i) (ItemQuery' i f) Slam (L.List i)

type ItemHTML = H.ComponentHTML (Const Void)

type RenderRec i s f = { path ∷ L.List i, html ∷ Array (HTML i s f) }

component
  ∷ ∀ a i s f
  . Ord i
  ⇒ ColumnOptions a i s f
  → Maybe (L.List i)
  → H.Component (State' a i s f) (Query' i f) Slam
component ispec initial =
  H.lifecycleParentComponent
    { render
    , eval
    , initializer: map (H.action ∘ Populate) initial
    , finalizer: Nothing
    , peek: Just peek
    }
  where

  render ∷ State a i → HTML i s f
  render { columns, selected } =
    let
      -- Drop the root item from `selected` (since there's no corresponding
      -- rendered item), and reverse it (foldl) to produce a list rather than a
      -- stack, so the steps can be zipped with the columns.
      selectSteps = A.fromFoldable (foldl go L.Nil (fromMaybe L.Nil (L.init selected)))
      go acc a = case acc of
        L.Nil → pure a
        _ → a : acc
    in
      HH.div
        [ HP.class_ (HH.className "sd-miller-columns")
        , HP.ref (H.action ∘ Ref)
        ]
        $ _.html
        $ foldl goColumn
            { path: L.Nil, html: [] }
            (A.zip columns (pad selectSteps (A.length columns)))

  goColumn
    ∷ RenderRec i s f
    → Tuple (Tuple i (L.List a)) (Maybe i)
    → RenderRec i s f
  goColumn acc (Tuple (Tuple item children) sel) =
    let path = item : acc.path
    in { path, html: acc.html <> [renderColumn path children sel] }

  renderColumn ∷ L.List i → L.List a → Maybe i → HTML i s f
  renderColumn colPath items selected =
    HH.div
      [ HP.class_ (HH.className "sd-miller-column")
      , ARIA.label "Column"
      , HP.ref (\_ → H.action Extended)
      ]
      [ renderSelected colPath selected items
      , HH.ul_ $ A.fromFoldable (renderItem colPath selected <$> items)
      ]

  renderSelected ∷ L.List i → Maybe i → L.List a → HTML i s f
  renderSelected colPath sel items =
    case L.find (\x → sel == Just (ispec.id x)) items of
      Nothing →
      HH.div
        [ HP.class_ (HH.className "sd-miller-column-selection") ]
        [ HH.span_ [ HH.text "No selection" ] ]
      Just x →
        let
          label = ispec.label x
          deselLabel = "Deselect '" <> label <> "'"
        in
          HH.div
            [ HP.classes
                [ HH.className "sd-miller-column-selection"
                , HH.className "selected"
                ]
            , HP.title deselLabel
            , ARIA.label deselLabel
            , HE.onClick $ HE.input_ (Populate colPath)
            ]
            [ HH.span
                [ HP.class_ (HH.className "sd-miller-column-selection-label") ]
                [ HH.text label ]
            , clearFieldIcon deselLabel
            ]

  renderItem ∷ L.List i → Maybe i → a → HTML i s f
  renderItem colPath selected item =
    let
      itemId = ispec.id item : colPath
      isSelected = Just (ispec.id item) == selected
    in
      HH.slot itemId \_ →
        ispec.render itemId item (if isSelected then Selected else Deselected)

  eval ∷ Query i ~> DSL a i s f
  eval (Ref mel next) = do
    H.modify (_ { element = mel })
    pure next
  eval (Extended next) = do
    traverse_ (H.fromEff ∘ scrollToRight) =<< H.gets _.element
    pure next
  eval (Populate path next) = do
    currentPath <- H.gets _.selected

    let
      prefix = findPathPrefix currentPath path
      remainder = L.take (L.length path - L.length prefix) path
      remPaths = scanr (:) prefix remainder
      deselected = L.take (L.length currentPath - L.length prefix) currentPath
      deselPaths = scanr (:) prefix deselected
      selected = L.take (L.length prefix + 1) path

    for_ deselPaths \i →
      H.query i $ left $ H.action $ ToggleSelected false

    H.modify \st → st
      { columns = A.take (L.length prefix) st.columns
      , selected = selected
      }

    -- Explicitly check whether anything needs loading before proceeding:
    -- incrementing `currentCycle` when there is no work to be done can
    -- break things if we're awaiting a previous load of the same path.
    when (not L.null remPaths) do
      H.modify \st → st { cycle = st.cycle + 1 }
      H.query selected $ left $ H.action $ ToggleSelected true
      currentCycle ← H.gets _.cycle
      raise' $ H.action $ Loading true
      more ← H.liftH $ H.liftH (parTraverse ispec.load remPaths)

      -- `load` is async, so in case multiple `Populate`s have been raised we need
      -- to check that we still care about the results of the load we just
      -- triggered.
      --
      -- The "cycle" counter is incremented every time a `load` is triggered, so
      -- if it differs after a `load` completes we know a competing `load` has
      -- been triggered in the mean time, and we can just abandon this result.
      --
      -- We can't just use `fork` here since we need to continue performing
      -- actions in `ComponentDSL` after the load completes, and there's no
      -- `MonadFork` instance for `ComponentDSL`.
      currentCycle' ← H.gets _.cycle
      when (currentCycle == currentCycle') do
        let
          newColumns = foldr go [] (L.zip remainder more)
          go (Tuple item colItems) cols =
            maybe cols (\items → cols <> [Tuple item items]) colItems
          selPaths = scanr (:) prefix path
        H.modify \st → st
          { columns = st.columns <> newColumns
          , selected = path
          }
        for_ (L.take (L.length remainder) selPaths) \i →
          H.query i $ left $ H.action $ ToggleSelected true
        raise' $ H.action $ Loading false

    pure next
  eval (Loading _ next) =
    pure next

  peek ∷ ∀ x. H.ChildF (L.List i) (ItemQuery' i f) x → DSL a i s f Unit
  peek = H.runChildF >>> flip coproduct (const (pure unit)) case _ of
      RaisePopulate path _ → do
        raise' $ H.action $ Populate path
      _ →
        pure unit

findPathPrefix ∷ ∀ a. Eq a ⇒ L.List a → L.List a → L.List a
findPathPrefix xs ys =
  let
    lxs = L.length xs
    lys = L.length ys
    xs' = if lxs > lys then L.drop (lxs - lys) xs else xs
    ys' = if lys > lxs then L.drop (lys - lxs) ys else ys
  in
    snd $ foldr go (Tuple true L.Nil) (L.zip xs' ys')
  where
  go (Tuple x y) (Tuple matching acc)
    | matching && x == y = Tuple true (x : acc)
    | otherwise = Tuple false acc

pad ∷ ∀ a. Array a → Int → Array (Maybe a)
pad xs length = map Just xs <> replicate (length - A.length xs) Nothing

scrollToRight ∷ ∀ eff. HTMLElement → Eff (dom ∷ DOM | eff) Unit
scrollToRight hel = do
  let el = htmlElementToElement hel
  maxScroll ← (-) <$> DOM.scrollWidth el <*> DOM.offsetWidth hel
  DOM.setScrollLeft maxScroll el
