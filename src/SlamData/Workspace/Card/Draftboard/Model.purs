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

module SlamData.Workspace.Card.Draftboard.Model
  ( Model
  , eqModel
  , genModel
  , emptyModel
  , encode
  , decode
  ) where

import SlamData.Prelude

import Data.Argonaut ((~>), (:=), (.?))
import Data.Argonaut as J
import Data.Codec.Argonaut.Compat as CA
import Data.Function (on)
import Data.Int (floor)
import Data.List as List
import Data.List(List(..), (:))
import Data.Map as Map
import Data.Maybe as Maybe
import Data.Ratio as Ratio
import Data.Rational ((%))
import Data.Rational as Rational
import Math as Math
import SlamData.Workspace.Card.Draftboard.Layout as Layout
import SlamData.Workspace.Card.Draftboard.Orientation as Orn
import SlamData.Workspace.Card.Draftboard.Pane as Pane
import SlamData.Workspace.Deck.DeckId as DID
import Test.StrongCheck.Arbitrary as SC
import Test.StrongCheck.Gen as Gen

type Model =
  { layout ∷ Pane.Pane (Maybe DID.DeckId)
  }

eqModel
  ∷ Model
  → Model
  → Boolean
eqModel m1 m2 =
  m1.layout ≡ m2.layout

genModel ∷ Gen.Gen Model
genModel =
  { layout: _ } <$> genPane 5

emptyModel ∷ Model
emptyModel = { layout: Pane.Cell Nothing }

encode
  ∷ Model
  → J.Json
encode m =
  "layout" := encodePane (CA.maybe DID.codec) m.layout
    ~> J.jsonEmptyObject

decode
  ∷ J.Json
  → Either String Model
decode j =
  J.decodeJson j >>= \obj → decodeLayout obj <|> decodeDecks j

  where
  decodeLayout obj = do
    layout ← decodePane (CA.maybe DID.codec) =<< obj .? "layout"
    pure { layout }

  decodeDecks obj = lmap CA.printJsonDecodeError do
    decks ← CA.decode (CA.map DID.codec codecDeckPosition) obj
    let
      layout = migrateLayout decks
    pure { layout }

encodePane
  ∷ ∀ a
  . CA.JsonCodec a
  → Pane.Pane a
  → J.Json
encodePane codecA = case _ of
  Pane.Cell a →
    "type" := "cell"
    ~> "value" := CA.encode codecA a
    ~> J.jsonEmptyObject
  Pane.Split orn ps →
    "type" := "split"
      ~> "orientation" := encodeOrientation orn
      ~> "panes" := map encodeSplit ps
      ~> J.jsonEmptyObject
  where
  encodeSplit (r × p) =
    "ratio" := J.encodeJson (Ratio.numerator r × Ratio.denominator r)
      ~> "pane" := encodePane codecA p
      ~> J.jsonEmptyObject

decodePane
  ∷ ∀ a
  . CA.JsonCodec a
  → J.Json
  → Either String (Pane.Pane a)
decodePane codecA =
  J.decodeJson >=> \obj → do
    ty ← obj .? "type"
    case ty of
      "cell" → do
        value ← lmap CA.printJsonDecodeError <<< CA.decode codecA =<< obj .? "value"
        pure (Pane.Cell value)
      "split" → do
        orn ← decodeOrientation =<< obj .? "orientation"
        ps ← traverse decodeSplits =<< obj .? "panes"
        pure (Pane.Split orn ps)
      _ → do
        Left ("Not a valid Pane tag: " <> ty)
  where
  decodeSplits obj = do
    n × d ← obj .? "ratio"
    pane ← decodePane codecA =<< obj .? "pane"
    pure (n%d × pane)

genPane
  ∷ ∀ a
  . SC.Arbitrary a
  ⇒ Int
  → Gen.Gen (Pane.Pane a)
genPane size = genOrientation >>= goGen size
  where
  goGen n orn = do
    bool ← SC.arbitrary
    if n ≡ 0 || bool
      then Pane.Cell <$> SC.arbitrary
      else Pane.Split orn <$> genSplit n orn 16 List.Nil

  genSplit n orn range ps = do
    num ← Gen.chooseInt 1 range
    pane ← goGen (n - 1) (Orn.reverse orn)
    let
      ps' = List.Cons ((num%16) × pane) ps
      range' = range - num
    if range' ≡ 0
      then pure ps'
      else genSplit n orn range' ps'

encodeOrientation ∷ Orn.Orientation → J.Json
encodeOrientation = J.encodeJson ∘ Orn.toString

decodeOrientation ∷ J.Json → Either String Orn.Orientation
decodeOrientation = J.decodeJson >=> case _ of
  "vertical" → pure Orn.Vertical
  "horizontal" → pure Orn.Horizontal
  s → Left ("Invalid orientation: " <> s)

genOrientation ∷ Gen.Gen Orn.Orientation
genOrientation = SC.arbitrary <#> if _ then Orn.Vertical else Orn.Horizontal

type DeckPosition =
  { x ∷ Number
  , y ∷ Number
  , width ∷ Number
  , height ∷ Number
  }

codecDeckPosition ∷ CA.JsonCodec DeckPosition
codecDeckPosition =
  CA.object "Deck position" $ CA.record
    # CA.recordProp (SProxy ∷ SProxy "x") CA.number
    # CA.recordProp (SProxy ∷ SProxy "y") CA.number
    # CA.recordProp (SProxy ∷ SProxy "width") CA.number
    # CA.recordProp (SProxy ∷ SProxy "height") CA.number

migrateLayout
  ∷ Map.Map DID.DeckId DeckPosition
  → Pane.Pane (Maybe DID.DeckId)
migrateLayout decks =
  splitTop total Nil deckList

  where
  deckList =
    List.sortBy (compare `on` (snd ⋙ \r → Math.sqrt ((r.x * r.x) + (r.y * r.y))))
      $ Map.toUnfoldable decks

  total =
    let
      size =
        foldl
          (\acc r →
            { width: if r.x + r.width > acc.width then r.x + r.width else acc.width
            , height: if r.y + r.height > acc.height then r.y + r.height else acc.height
            })
          { width: 0.0, height: 0.0 }
          decks
    in case deckList of
      List.Cons (_ × rect) _ →
        { x: rect.x
        , y: rect.y
        , width: size.width - rect.x
        , height: size.height - rect.y
        }
      _ →
        { x: 0.0
        , y: 0.0
        , width: 0.0
        , height: 0.0
        }

  splitTop r1 Nil Nil =
    Pane.Cell Nothing
  splitTop r1 ps Nil =
    splitLeft r1 Nil ps
  splitTop r1 Nil ((deckId × r2) : Nil) =
    Pane.Cell (Just deckId)
  splitTop r1 ps ((deckId × r2) : ds) | r2.y - r1.y == 0.0 =
    splitTop r1 ((deckId × r2) : ps) ds
  splitTop r1 ps ((deckId × r2) : ds) =
    case trySplitV r2.y (ps <> ds) of
      Just (as × bs) →
        let
          ratio = floor (r2.y - r1.y) % floor r1.height
          r1'   = r1 { height = r2.y - r1.y }
        in case bs of
          Nil →
            merge Orn.Vertical ratio
              (splitLeft r1' Nil as)
              (Pane.wrap Orn.Horizontal (Pane.Cell (Just deckId)))
          _ →
            merge Orn.Vertical ratio
              (splitLeft r1' Nil as)
              (splitLeft r1' Nil ((deckId × r2) : bs))
      Nothing →
        splitTop r1 ((deckId × r2) : ps) ds

  splitLeft r1 Nil Nil =
    Pane.Cell Nothing
  splitLeft r1 ps Nil =
    splitTop r1 Nil ps
  splitLeft r1 Nil ((deckId × r2) : Nil) =
    Pane.Cell (Just deckId)
  splitLeft r1 ps ((deckId × r2) : ds) | r2.x - r1.x == 0.0 =
    splitLeft r1 ((deckId × r2) : ps) ds
  splitLeft r1 ps ((deckId × r2) : ds) =
    case trySplitH r2.x (ps <> ds) of
      Just (as × bs) →
        let
          ratio = floor (r2.x - r1.x) % floor r1.width
          r1'   = r1 { width = r2.x - r1.x }
        in case bs of
          Nil →
            merge Orn.Horizontal ratio
              (splitTop r1' Nil as)
              (Pane.wrap Orn.Vertical (Pane.Cell (Just deckId)))
          _ →
            merge Orn.Horizontal ratio
              (splitTop r1' Nil as)
              (splitTop r1' Nil ((deckId × r2) : bs))
      Nothing →
        splitLeft r1 ((deckId × r2) : ps) ds

  trySplitH = trySplit Orn.Horizontal Nil Nil
  trySplitV = trySplit Orn.Vertical Nil Nil

  trySplit orn a b z Nil = Just (a × b)
  trySplit orn a b z ((deckId × r) : ds) =
    case orn of
      Orn.Horizontal →
        if z > r.x && z < r.x + r.width then
          Nothing
        else if r.x < z then
          trySplit orn ((deckId × r) : a) b z ds
        else
          trySplit orn a ((deckId × r) : b) z ds
      Orn.Vertical →
        if z > r.y && z < r.y + r.height then
          Nothing
        else if r.y < z then
          trySplit orn ((deckId × r) : a) b z ds
        else
          trySplit orn a ((deckId × r) : b) z ds

  merge orn ratio a b =
    let
      ps = List.fromFoldable [ ratio × Pane.Cell Nothing, (one - ratio) × Pane.Cell Nothing ]
    in case (Layout.defaultMerge orn ps 0 a), (Layout.defaultMerge orn ps 1 b) of
      Pane.Split _ as, Pane.Split _ bs →
        let
          as' = unsafePartial (Maybe.fromJust (List.init as))
          bs' = unsafePartial (Maybe.fromJust (List.tail bs))
        in
          Pane.Split orn (bestFit zero Nil (as' <> bs'))
      _, _ →
        -- Should never happen
        Pane.Split orn ps

  bestFit off acc Nil = Nil
  bestFit off acc ((_ × p) : Nil) =
    List.reverse ((one - off × p) : acc)
  bestFit off acc ((ratio × p) : ps) =
    let
      ratio' = Layout.closestSnapRatio (Rational.toNumber ratio)
    in
      bestFit (off + ratio') ((ratio' × p) : acc) ps
