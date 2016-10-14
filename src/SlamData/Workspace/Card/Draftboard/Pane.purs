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

module SlamData.Workspace.Card.Draftboard.Pane
  ( Pane(..)
  , Locator(..)
  , Cursor
  , wrap
  , children
  , rootLocator
  , walkWithCursor
  , walkWithLocator
  , runCursorWithLocator
  , runCursor
  , getAt
  , getAtWithLocator
  , getValueAt
  , modifyAt
  , toList
  , traverseWithCursor
  , withCursor
  ) where

import SlamData.Prelude

import Data.List (List(..), (:))
import Data.List as List
import Data.Rational (Rational)

import SlamData.Workspace.Card.Draftboard.Orientation (Orientation(..))

data Pane a
  = Split Orientation (List (Rational × Pane a))
  | Cell a

data Locator
  = Locator Cursor Rational Rational Rational Rational Rational Rational
  --               x        y        width    height   offset   ratio

type Cursor = List Int

-- | Wraps a Pane in a Split.
wrap ∷ ∀ a. Orientation → Pane a → Pane a
wrap orn pane = Split orn (List.singleton (one × pane))

-- | Gets the immediate children of a Pane. Cells have no children.
children ∷ ∀ a. Pane a → List (Pane a)
children = case _ of
  Cell _ → List.Nil
  Split _ as → map snd as

-- | A root locator represents 100% of the available space.
rootLocator ∷ Locator
rootLocator = Locator Nil zero zero one one zero one

-- | Fold over every node with it's cursor
walkWithCursor
  ∷ ∀ a r
  . (Cursor → r → Pane a → r)
  → r
  → Pane a
  → r
walkWithCursor f = go Nil
  where
  go c acc p =
    case p of
      Cell a → f c acc p
      Split _ ps →
        goIx c 0 acc ps

  goIx c ix acc ps =
    case ps of
      Nil → acc
      (_ × p) : ps' →
        goIx c (ix + 1) (go (ix : c) acc p) ps'

-- | Fold over every node with it's location.
walkWithLocator
  ∷ ∀ a r
  . (Locator → r → Pane a → r)
  → r
  → Pane a
  → r
walkWithLocator f = go rootLocator
  where
  go loc@(Locator c x y w h _ _) acc p =
    let
      acc' =
        case p of
          Cell _ → acc
          Split orn ps →
            goIx x y w h zero 0 orn acc ps
    in
      f loc acc' p

    where
    goIx x y w h off ix orn acc ps =
      case ps of
        Nil → acc
        (r × p) : ps' →
          case orn of
            Horizontal →
              goIx (r * w + x) y w h (off + r) (ix + 1) orn
                (go (Locator (ix : c) x y (r * w) h off r) acc p) ps'
            Vertical →
              goIx x (r * h + y) w h (off + r) (ix + 1) orn
                (go (Locator (ix : c) x y w (r * h) off r) acc p) ps'

-- | Fold over a cursor (leaf to root).
runCursor
  ∷ ∀ a r
  . (Pane a → r)
  → (Orientation → List (Rational × Pane a) → Int → r → r)
  → Cursor
  → Pane a
  → Maybe r
runCursor f g = foldl go (Just ∘ f)
  where
  go h ix p =
    case p of
      Cell _ → Nothing
      Split orn ps → do
        p' ← snd <$> List.index ps ix
        g orn ps ix <$> h p'

-- | Fold over a cursor (leaf to root) with a location for each node.
runCursorWithLocator
  ∷ ∀ a r
  . (Locator → Pane a → r)
  → (Locator → Orientation → List (Rational × Pane a) → Int → r → r)
  → Cursor
  → Pane a
  → Maybe r
runCursorWithLocator f g = go rootLocator <<< List.reverse
  where
  go loc@(Locator c x y w h _ _) cursor pane =
    case cursor of
      Nil → Just (f loc pane)
      ix : cs →
        case pane of
          Cell a → Nothing
          Split orn ps →
            g loc orn ps ix <$> goIx zero 0 ix orn cs ps
    where
    goIx off i ix orn cs ps =
      case ps of
        (r × p) : ps' | i == ix →
          case orn of
            Horizontal →
              go (Locator (ix : c) (off * w + x) y (r * w) h off r) cs p
            Vertical →
              go (Locator (ix : c) x (off * h + y) w (r * h) off r) cs p
        (r × p) : ps' →
          goIx (off + r) (i + 1) ix orn cs ps'
        _ → Nothing

-- | Retrieves a Pane represented by a cursor.
getAt ∷ ∀ a . Cursor → Pane a → Maybe (Pane a)
getAt = runCursor id (\_ _ _ r → r)

getAtWithLocator ∷ ∀ a. Cursor → Pane a → Maybe (Locator × Pane a)
getAtWithLocator = runCursorWithLocator Tuple (\_ _ _ _ r → r)

-- | Retrieves a Cell value represented by a cursor. Splits have no value.
getValueAt ∷ ∀ a . Cursor → Pane a → Maybe a
getValueAt c p =
  getAt c p >>= case _ of
    Cell a → pure a
    _      → Nothing

-- | Modifies a Pane represented by a cursor.
modifyAt ∷ ∀ a. (Pane a → Pane a) → Cursor → Pane a → Maybe (Pane a)
modifyAt = flip runCursor go
  where
  go orn ps ix r =
    Split orn (fromMaybe ps (List.modifyAt ix (map (const r)) ps))

instance eqPane ∷ Eq a ⇒ Eq (Pane a) where
  eq (Cell a) (Cell b) = eq a b
  eq (Split orn1 ps1) (Split orn2 ps2) = eq orn1 orn2 && eq ps1 ps2
  eq _ _ = false

instance functorPane ∷ Functor Pane where
  map f (Cell a) = Cell (f a)
  map f (Split orn as) = Split orn (map (map (map f)) as)

instance foldablePane ∷ Foldable Pane where
  foldMap f (Cell a) = f a
  foldMap f (Split orn as) = foldMap (foldMap f <<< snd) as

  foldl f acc (Cell a) = f acc a
  foldl f acc (Split orn as) = foldl (\acc' p → foldl f acc' (snd p)) acc as

  foldr f acc (Cell a) = f a acc
  foldr f acc (Split orn as) = foldr (\p acc' → foldr f acc' (snd p)) acc as

instance traversablePane ∷ Traversable Pane where
  traverse f (Cell a) = Cell <$> f a
  traverse f (Split orn as) = Split orn <$> traverse (traverse (traverse f)) as

  sequence (Cell a) = Cell <$> a
  sequence (Split orn as) = Split orn <$> traverse (traverse sequence) as

toList ∷ ∀ a. Pane a → List a
toList = walkWithCursor go Nil
  where
  go _ r (Cell a) = a : r
  go _ r _ = r

traverseWithCursor
  ∷ ∀ f a b
  . Applicative f
  ⇒ (Cursor → a → f b)
  → Pane a
  → f (Pane b)
traverseWithCursor = go Nil
  where
  go c f (Cell a) = Cell <$> f c a
  go c f (Split orn as) = Split orn <$> sequence (goIx c f 0 Nil as)

  goIx c f i acc Nil = List.reverse acc
  goIx c f i acc (a : as) = goIx c f (i + 1) (traverse (go (i : c) f) a : acc) as

withCursor
  ∷ ∀ a
  . Pane a
  → List (Cursor × a)
withCursor = walkWithCursor go Nil
  where
  go c r (Cell a) = (c × a) : r
  go _ r _ = r
