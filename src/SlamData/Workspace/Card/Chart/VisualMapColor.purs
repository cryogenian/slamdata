module SlamData.Workspace.Card.Chart.VisualMapColor where

import SlamData.Prelude

import Data.Argonaut (fromString, class EncodeJson, class DecodeJson, decodeJson)

import SlamData.Form.Select (class OptionVal)

import Test.StrongCheck.Arbitrary (class Arbitrary)
import Test.StrongCheck.Gen as Gen


data VisualMapColor
  = Blue
  | Purple
  | Orange
  | Red
  | Green
  | Yellow
  | White
  | Black

allVisualMapColors ∷ Array VisualMapColor
allVisualMapColors =
  [ Blue
  , Purple
  , Orange
  , Red
  , Green
  , Yellow
  , White
  , Black
  ]

printVisualMapColor ∷ VisualMapColor → String
printVisualMapColor = case _ of
  Blue → "Blue"
  Purple → "Purple"
  Orange → "Orange"
  Red → "Red"
  Green → "Green"
  Yellow → "Yellow"
  White → "White"
  Black → "Black"

parseVisualMapColor ∷ String → String ⊹ VisualMapColor
parseVisualMapColor = case _ of
  "Blue" → pure Blue
  "Purple" → pure Purple
  "Orange" → pure Orange
  "Red" → pure Red
  "Green" → pure Green
  "Yellow" → pure Yellow
  "White" → pure White
  "Black" → pure Black
  _ → Left "Incorrect visual map color string"

derive instance genericVisualMapColor ∷ Generic VisualMapColor
derive instance eqVisualMapColor ∷ Eq VisualMapColor
derive instance ordVisualMapColor ∷ Ord VisualMapColor

instance encodeJsonVisualMapColor ∷ EncodeJson VisualMapColor where
  encodeJson = fromString ∘ printVisualMapColor
instance decodeJsonVisualMapColor ∷ DecodeJson VisualMapColor where
  decodeJson = decodeJson >=> parseVisualMapColor

instance arbitraryVisualMapColor ∷ Arbitrary VisualMapColor where
  arbitrary = Gen.allInArray allVisualMapColors

instance optionValVisualMapColor ∷ OptionVal VisualMapColor where
  stringVal = printVisualMapColor
