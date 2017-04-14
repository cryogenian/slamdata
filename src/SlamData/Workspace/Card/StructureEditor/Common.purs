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

module SlamData.Workspace.Card.StructureEditor.Common where

import SlamData.Prelude

import Data.Array as A
import Data.Eq (class Eq1, eq1)
import Data.Functor.Coproduct (Coproduct(..))
import Data.Functor.Mu (Mu)
import Data.Int as Int
import Data.Json.Extended as EJ
import Data.Json.Extended.Cursor as EJC
import Data.List ((:))
import Data.List as L
import Data.Map as M
import Data.Newtype as N
import Data.Ord (class Ord1, compare1)
import Data.Path.Pathy as Path

import Matryoshka (class Corecursive, class Recursive, Algebra, cata, embed, project, transCata)

import SlamData.Quasar.Query as QQ
import SlamData.Workspace.MillerColumns.Column.BasicFilter (mkFilter)
import SlamData.Workspace.MillerColumns.Component as MC

import Utils.Path as PU

-- Helper until we have Eq1 / Ord1 instances for Coproduct in the core lib
newtype CP1 f g a = CP1 (Coproduct f g a)

derive newtype instance functorCP1 ∷ (Functor f, Functor g) ⇒ Functor (CP1 f g)

instance eq1CP1 ∷ (Eq1 f, Eq1 g) ⇒ Eq1 (CP1 f g) where
  eq1 (CP1 (Coproduct x)) (CP1 (Coproduct y)) =
    case x, y of
      Left a, Left b → eq1 a b
      Right a, Right b → eq1 a b
      _, _ → false

instance ord1CP1 ∷ (Ord1 f, Ord1 g) ⇒ Ord1 (CP1 f g) where
  compare1 (CP1 (Coproduct x)) (CP1 (Coproduct y)) =
    case x, y of
      Left a, Left b → compare1 a b
      Left _, _ → LT
      _, Left _ → GT
      Right a, Right b → compare1 a b

toCP1 :: Mu (Coproduct EJC.CursorF ECursorF) → Mu (CP1 EJC.CursorF ECursorF)
toCP1 = transCata CP1

data ECursorF a = OfValue EJ.EJson a

derive instance functorECursorF ∷ Functor ECursorF

derive instance eqECursorF ∷ Eq a ⇒ Eq (ECursorF a)
instance eq1ECursorF ∷ Eq1 ECursorF where eq1 = eq

derive instance ordECursorF ∷ Ord a ⇒ Ord (ECursorF a)
instance ord1ECursorF ∷ Ord1 ECursorF where compare1 = compare

newtype Cursor = Cursor (Mu (Coproduct EJC.CursorF ECursorF))

derive instance newtypeCursor ∷ Newtype Cursor _

instance corecursiveCursor ∷ Corecursive Cursor (Coproduct EJC.CursorF ECursorF) where
  embed = N.collect Cursor embed

instance recursiveCursor ∷ Recursive Cursor (Coproduct EJC.CursorF ECursorF) where
  project = N.traverse Cursor project

instance eqCursor :: Eq Cursor where
  eq (Cursor x) (Cursor y) = eq (toCP1 x) (toCP1 y)

instance ordCursor :: Ord Cursor where
  compare (Cursor x) (Cursor y) = compare (toCP1 x) (toCP1 y)

all :: Cursor
all = embed $ left $ EJC.All

atKey ∷ EJ.EJson → Cursor → Cursor
atKey k = embed <<< left <<< EJC.AtKey k

atIndex ∷ Int → Cursor → Cursor
atIndex i = embed <<< left <<< EJC.AtIndex i

ofValue ∷ EJ.EJson → Cursor → Cursor
ofValue v = embed <<< right <<< OfValue v

get ∷ Cursor → EJ.EJson → Maybe EJ.EJson
get = cata go
  where
  go ∷ Algebra (Coproduct EJC.CursorF ECursorF) (EJ.EJson → Maybe EJ.EJson)
  go = unwrap >>> case _ of
    Left EJC.All → Just
    Left (EJC.AtKey k prior) → EJC.getKey k <=< prior
    Left (EJC.AtIndex i prior) → EJC.getIndex i <=< prior
    Right (OfValue val prior) → (\val' → if val == val' then Just val else Nothing) <=< prior

type ColumnPath = Cursor

rootColumn ∷ ColumnPath
rootColumn = all

printCursor ∷ Cursor → String
printCursor path = case unwrap (project path) of
  Left EJC.All → "*"
  Left (EJC.AtKey k _) → "*." <> EJ.renderEJson k <> ""
  Left (EJC.AtIndex i _) → "*[" <> show i <> "]"
  Right (OfValue val _) → EJ.renderEJson val

data ColumnItem = ColumnItem Cursor Weight

derive instance eqColumnItem ∷ Eq ColumnItem
derive instance ordColumnItem ∷ Ord ColumnItem

newtype Weight = Weight Number

derive instance eqWeight ∷ Eq Weight
derive instance ordWeight ∷ Ord Weight
derive instance newtypeWeight ∷ Newtype Weight _

columnItemLabel ∷ ColumnItem → String
columnItemLabel (ColumnItem path _) = printCursor path

columnItemPath ∷ ColumnItem → ColumnPath
columnItemPath (ColumnItem path _) = path

columnItemWeight ∷ ColumnItem → Weight
columnItemWeight (ColumnItem _ w) = w

columnPathIsLeaf ∷ ColumnPath → Boolean
columnPathIsLeaf path = case unwrap (project path) of
  Left _ → false
  Right _ → true

analyse ∷ Array EJ.EJson → ColumnPath → L.List ColumnItem
analyse items path =
  let
    items' = A.mapMaybe (get path) items
  in
    map (uncurry ColumnItem)
      $ M.toAscUnfoldable
      $ countFreq (A.length items')
      $ foldl go L.Nil items'
  where
  go :: L.List Cursor -> EJ.EJson -> L.List Cursor
  go acc item = case project item of
    EJ.Map (EJ.EJsonMap kvs) →
      foldl (\acc' (Tuple k _) → atKey k path : acc') acc kvs
    EJ.Array xs →
      foldl (\acc' x → x : acc') acc $
        A.mapWithIndex (\i _ → atIndex i path) xs
    x →
      ofValue item path : acc

countFreq ∷ ∀ f a. (Foldable f, Ord a) ⇒ Int → f a → M.Map a Weight
countFreq total = compute ∘ foldl (flip go) M.empty
  where
  go ∷ a → M.Map a Int → M.Map a Int
  go = M.alter (Just ∘ maybe 1 (_ + 1))
  compute ∷ M.Map a Int → M.Map a Weight
  compute = map (\count → Weight $ Int.toNumber count / Int.toNumber total)

load
  ∷ ∀ m
  . Monad m
  ⇒ QQ.QuasarDSL m
  ⇒ ColumnPath
  → MC.LoadRequest
  → Maybe PU.FilePath
  → m (MC.LoadResponse ColumnItem)
load path { requestId, filter } =
  case _ of
    Just resource → do
      case (fst <$> Path.peel resource) of
        Just resourcePath → do
          let sql = QQ.templated resource "SELECT * FROM {{path}} AS row LIMIT 1000"
          QQ.queryEJson resourcePath sql >>= case _ of
            Left _ →
              pure noResult
            Right records →
              let items = L.filter (mkFilter filter ∘ columnItemLabel) (analyse records path)
              in pure { requestId, items, nextOffset: Nothing }
        _ → pure noResult
    _ → pure noResult
  where
  noResult = { requestId, items: L.Nil, nextOffset: Nothing }
