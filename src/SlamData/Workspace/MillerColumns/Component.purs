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
  ( ItemSpec
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
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Component.Utils (raise)

import SlamData.Monad (Slam)
import SlamData.Workspace.MillerColumns.Component.Query (Query(..))
import SlamData.Workspace.MillerColumns.Component.State (State, initialState)

type ItemSpec a i m =
  { label ∷ a → String
  , render ∷ a → H.ComponentHTML (Const Void)
  , load ∷ L.List i → m (Maybe (L.List a))
  , id ∷ a → i
  }

type HTML i = H.ComponentHTML (Query i)
type DSL a i m p = H.ComponentDSL (State a i) (Query i) m p

type ItemHTML = H.ComponentHTML (Const Void)

type RenderRec i = { path ∷ L.List i, html ∷ Array (HTML i) }

component
  ∷ ∀ a i
  . Eq i
  ⇒ ItemSpec a i Slam
  → Maybe (L.List i)
  → H.Component (State a i) (Query i) Slam
component ispec initial =
  H.lifecycleComponent
    { render
    , eval
    , initializer: map (H.action ∘ Populate) initial
    , finalizer: Nothing
    }
  where

  render ∷ State a i → HTML i
  render { columns, selected } =
    let
      -- Drop the root item from `selected` (since there's no corresponding
      -- rendered item), and reverse it (foldl) to produce a list rather than a
      -- stack, so the steps can be zipped with the columns. The fold also tags
      -- the steps as `Left` or `Right`, where `Right` is the "tip", so we can
      -- add a class to the tip-selected-item as necessary.
      selectSteps = A.fromFoldable (foldl go L.Nil (fromMaybe L.Nil (L.init selected)))
      go acc a = case acc of
        L.Nil → pure (Right a)
        _ → Left a : acc
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
    ∷ RenderRec i
    → Tuple (Tuple i (L.List a)) (Maybe (Either i i))
    → RenderRec i
  goColumn acc (Tuple (Tuple item children) sel) =
    let path = item : acc.path
    in { path, html: acc.html <> [renderColumn path children sel] }

  renderColumn ∷ L.List i → L.List a → Maybe (Either i i) → HTML i
  renderColumn colPath items selected =
    HH.ul
      [ HP.class_ (HH.className "sd-miller-column")
      , ARIA.label "Column"
      , HP.ref (\_ → H.action Extended)
      ]
      $ A.fromFoldable (renderItem colPath selected <$> items)

  renderItem ∷ L.List i → Maybe (Either i i) → a → HTML i
  renderItem colPath selected item =
    let
      label = ispec.label item
      isSelected = Just (ispec.id item) == (either id id <$> selected)
      isTip = isSelected && maybe false isRight selected
    in
      HH.li
        [ HE.onClick $ HE.input_ $ Populate (ispec.id item : colPath)
        , HP.title label
        , ARIA.label ("Select " <> label)
        , HP.classes
            $ (guard isSelected $> HH.className "selected")
            <> (guard isTip $> HH.className "tip")
        ]
        [ absurd ∘ getConst <$> ispec.render item ]

  eval ∷ Query i ~> DSL a i Slam
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

    H.modify \st → st
      { cycle = st.cycle + 1
      , columns = A.take (L.length prefix) st.columns
      , selected = L.take (L.length prefix + 1) path
      }

    currentCycle ← H.gets _.cycle

    raise $ H.action $ Loading true

    more ← H.liftH (parTraverse ispec.load remPaths)

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
          maybe cols (\items -> cols <> [Tuple item items]) colItems
      H.modify \st → st
        { columns = st.columns <> newColumns
        , selected = path
        }
      raise $ H.action $ Loading false

    pure next
  eval (Loading _ next) =
    pure next

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
