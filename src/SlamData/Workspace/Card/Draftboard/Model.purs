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
import Data.Argonaut as J
import Data.Argonaut ((~>), (:=), (.?))
import Data.Int (toNumber)
import Data.List as List
import Data.Ratio as Ratio
import Data.Rational (Rational(..), (%))
import SlamData.Workspace.Card.Draftboard.Orientation as Orn
import SlamData.Workspace.Card.Draftboard.Pane as Pane
import SlamData.Workspace.Deck.DeckId (DeckId)
import Test.StrongCheck.Arbitrary as SC
import Test.StrongCheck.Gen as Gen

type Model =
  { layout ∷ Pane.Pane (Maybe DeckId)
  }

eqModel
  ∷ Model
  → Model
  → Boolean
eqModel m1 m2 =
  m1.layout ≡ m2.layout

genModel ∷ Gen.Gen Model
genModel =
  { layout: _ } <$> genPane

emptyModel ∷ Model
emptyModel = { layout: Pane.Cell Nothing }

encode
  ∷ Model
  → J.Json
encode m =
  "layout" := encodePane m.layout
    ~> J.jsonEmptyObject

decode
  ∷ J.Json
  → Either String Model
decode =
  J.decodeJson >=> \obj → do
    layout ← decodePane =<< obj .? "layout"
    pure { layout }

encodePane
  ∷ ∀ a
  . J.EncodeJson a
  ⇒ Pane.Pane a
  → J.Json
encodePane = case _ of
  Pane.Cell a →
    "type" := "cell"
    ~> "value" := J.encodeJson a
    ~> J.jsonEmptyObject
  Pane.Split orn ps →
    "type" := "split"
      ~> "orientation" := encodeOrientation orn
      ~> "panes" := map encodeSplit ps
      ~> J.jsonEmptyObject
  where
  encodeSplit (Rational r × p) =
    "ratio" := J.encodeJson (Ratio.numerator r × Ratio.denominator r)
      ~> "pane" :=  encodePane p
      ~> J.jsonEmptyObject

decodePane
  ∷ ∀ a
  . J.DecodeJson a
  ⇒ J.Json
  → Either String (Pane.Pane a)
decodePane =
  J.decodeJson >=> \obj → do
    ty ← obj .? "type"
    case ty of
      "cell" → do
        value ← obj .? "value"
        pure (Pane.Cell value)
      "split" → do
        orn ← decodeOrientation =<< obj .? "orientation"
        ps ← traverse decodeSplits =<< obj .? "panes"
        pure (Pane.Split orn ps)
      _ →
        Left ("Not a valid Pane tag: " <> ty)
  where
  decodeSplits obj = do
    n × d ← obj .? "ratio"
    pane ← decodePane =<< obj .? "pane"
    pure (n%d × pane)

genPane
  ∷ ∀ a
  . SC.Arbitrary a
  ⇒ Gen.Gen (Pane.Pane a)
genPane = genOrientation >>= goGen
  where
  goGen orn =
    SC.arbitrary >>=
    if _
      then Pane.Cell <$> SC.arbitrary
      else Pane.Split orn <$> genSplit orn 16 List.Nil

  genSplit orn range ps = do
    num ← Gen.chooseInt 1.0 (toNumber range)
    pane ← goGen (Orn.reverse orn)
    let
      ps' = List.Cons ((num%16) × pane) ps
      range' = range - num
    if range' ≡ 0
      then pure ps'
      else genSplit orn range' ps'

encodeOrientation ∷ Orn.Orientation → J.Json
encodeOrientation = J.encodeJson ∘ Orn.toString

decodeOrientation ∷ J.Json → Either String Orn.Orientation
decodeOrientation = J.decodeJson >=> case _ of
  "vertical" → pure Orn.Vertical
  "horizontal" → pure Orn.Horizontal
  s → Left ("Invalid orientation: " <> s)

genOrientation ∷ Gen.Gen Orn.Orientation
genOrientation = SC.arbitrary <#> if _ then Orn.Vertical else Orn.Horizontal
