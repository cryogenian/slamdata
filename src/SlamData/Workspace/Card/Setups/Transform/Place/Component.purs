{-
Copyright 2017 SlamData, Inc.

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

module SlamData.Workspace.Card.Setups.Transform.Place.Component where

import SlamData.Prelude
import Control.Monad.Eff.Random (randomInt)
import Data.Array as Array
import Data.Foldable as F
import Data.Lens as Lens
import Data.Ord (abs)
import Data.Profunctor (dimap)
import Halogen as H
import Halogen.Component.Profunctor (ProComponent(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import SlamData.Monad (Slam)
import SlamData.Workspace.Card.Setups.Transform as T
import SlamData.Workspace.Card.Setups.Transform.Numeric as N
import Utils (stringToInt, chunksOf)

data Query a
  = Init a
  | ChangePlace Int a

type State =
  { left ∷ Array String
  , right ∷ Array String
  , place ∷ Int
  }

type DSL = H.ComponentDSL State Query (Maybe N.Place) Slam
type HTML = H.ComponentHTML Query

component ∷ ProComponent HH.HTML Query Slam (Maybe N.Place) (Maybe N.Place)
component =
  ProComponent $ H.lifecycleComponent
    { initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just (H.action Init)
    , finalizer: Nothing
    }
  where
  initialState ∷ Maybe (N.Place) → State
  initialState = { left: [], right: [], place: _ } ∘ case _ of
    Just (N.Place n) → clamp (-10) 10 n
    Nothing → 0

  render ∷ State → HTML
  render st =
    HH.div
      [ HP.classes [ HH.ClassName "sd-place-value-picker" ] ]
      [ HH.p
          [ HP.classes [ HH.ClassName "place-value" ] ]
          (renderNumber st.left st.right st.place)
      , HH.p_
          [ HH.input
              [ HP.type_ HP.InputRange
              , HP.prop (HH.PropName "min") (-10)
              , HP.prop (HH.PropName "max") 10
              , HP.value (show (-st.place))
              , HE.onValueInput (map (H.action ∘ ChangePlace ∘ negate) ∘ stringToInt)
              ]
          ]
      ]

  bold ∷ String → HTML
  bold a = HH.b_ [ HH.text a ]

  renderNumber ∷ Array String → Array String → Int → Array HTML
  renderNumber ls rs = case _ of
    n | n < 0 →
      let
        head = Array.slice (abs n) (abs n + 1) rs
        leading = Array.reverse (Array.take 3 ls)
        decimals = Array.take (abs n - 1) rs
      in
        map HH.text leading
        <> [ HH.text "." ]
        <> map HH.text decimals
        <> map bold head
        <> [ HH.text "0" ]
    n →
      let
        pad = Array.replicate n "0"
        head = Array.slice n (n + 1) rs
        leading = Array.slice (n + 1) (n + 3) rs
        nums = map HH.text pad <> map bold head <> map HH.text leading
        chunks = chunksOf 3 nums
        commas = Array.reverse (F.intercalate [ HH.text "," ] chunks)
      in
        commas <> [ HH.text ".0" ]

  eval ∷ Query ~> DSL
  eval = case _ of
    Init next → do
      numbers ← map show <$> genNumbers
      H.modify _
        { left = Array.take 16 numbers
        , right = Array.drop 16 numbers
        }
      pure next
    ChangePlace n next → do
      numbers ← map show <$> genNumbers
      H.modify _
        { left = Array.take 16 numbers
        , right = Array.drop 16 numbers
        , place = n
        }
      H.raise (Just (N.Place n))
      pure next

genNumbers ∷ DSL (Array Int)
genNumbers = H.liftEff $ sequence (Array.replicate 32 (randomInt 1 9))

transformFloor ∷ H.Component HH.HTML Query (Maybe T.Transform) (Maybe T.Transform) Slam
transformFloor =
  unwrap $ dimap
    (Lens.preview (Lens._Just ∘ T._Numeric ∘ N._Floor))
    (map (T.Numeric ∘ N.Floor))
    component

transformRound ∷ H.Component HH.HTML Query (Maybe T.Transform) (Maybe T.Transform) Slam
transformRound =
  unwrap $ dimap
    (Lens.preview (Lens._Just ∘ T._Numeric ∘ N._Round))
    (map (T.Numeric ∘ N.Round))
    component

transformCeil ∷ H.Component HH.HTML Query (Maybe T.Transform) (Maybe T.Transform) Slam
transformCeil =
  unwrap $ dimap
    (Lens.preview (Lens._Just ∘ T._Numeric ∘ N._Ceil))
    (map (T.Numeric ∘ N.Ceil))
    component
