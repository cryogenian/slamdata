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
  { digits ∷ Array Int
  , place ∷ Int
  }

type DSL = H.ComponentDSL State Query (Maybe N.Place) Slam
type HTML = H.ComponentHTML Query

component ∷ (Int → Int → Int) → ProComponent HH.HTML Query Slam (Maybe N.Place) (Maybe N.Place)
component operation =
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
  initialState = { digits: [], place: _ } ∘ case _ of
    Just (N.Place n) → clamp (-10) 10 n
    Nothing → 0

  render ∷ State → HTML
  render st =
    HH.div
      [ HP.classes [ HH.ClassName "sd-place-value-picker" ] ]
      [ HH.label_ [ HH.text "Place" ]
      , HH.p
          [ HP.classes [ HH.ClassName "place-value" ] ]
          (renderNumber st.digits st.place)
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

  renderNumber ∷ Array Int → Int → Array HTML
  renderNumber ds n =
    let
      pivotIx = 10 + n
      digitIx = pivotIx + 1
      fn      = operation $ fromMaybe 0 $ Array.index ds pivotIx
      ds'     = flip Array.mapWithIndex ds case _, _ of
                  ix, val | ix ≡ digitIx → bold $ show (fn val)
                          | ix < digitIx → HH.text "0"
                          | otherwise    → HH.text $ show val
      nths    = Array.reverse $ Array.take 11 ds'
      ns      = Array.reverse $ F.intercalate [ HH.text "," ] $ chunksOf 3 (Array.drop 11 ds')
    in
      ns <> [ HH.text "." ] <> nths

  eval ∷ Query ~> DSL
  eval = case _ of
    Init next → do
      digits ← genDigits
      H.modify _ { digits = digits }
      pure next
    ChangePlace n next → do
      H.modify _ { place = n }
      H.raise (Just (N.Place n))
      pure next

genDigits ∷ DSL (Array Int)
genDigits =
  -- We only do 1 - 8 to simplify the operation
  H.liftEff $ sequence (Array.replicate 23 (randomInt 1 8))

transformFloor ∷ H.Component HH.HTML Query (Maybe T.Transform) (Maybe T.Transform) Slam
transformFloor =
  unwrap $ dimap
    (Lens.preview (Lens._Just ∘ T._Numeric ∘ N._Floor))
    (map (T.Numeric ∘ N.Floor))
    (component (const id))

transformRound ∷ H.Component HH.HTML Query (Maybe T.Transform) (Maybe T.Transform) Slam
transformRound =
  unwrap $ dimap
    (Lens.preview (Lens._Just ∘ T._Numeric ∘ N._Round))
    (map (T.Numeric ∘ N.Round))
    (component \n → if n < 5 then id else (_ + 1))

transformCeil ∷ H.Component HH.HTML Query (Maybe T.Transform) (Maybe T.Transform) Slam
transformCeil =
  unwrap $ dimap
    (Lens.preview (Lens._Just ∘ T._Numeric ∘ N._Ceil))
    (map (T.Numeric ∘ N.Ceil))
    (component (const (_ + 1)))
