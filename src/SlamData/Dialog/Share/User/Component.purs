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

module SlamData.Dialog.Share.User.Component where

import SlamData.Prelude

import Data.Foldable as F
import Data.Lens (LensP, lens, (.~), (%~), (?~))
import Data.Lens.Index (ix)
import Data.Map as Map
import Data.Time (Milliseconds(..))

import DOM.HTML.Types (HTMLElement)

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B
import Halogen.CustomProps as CP
import Halogen.Component.Utils (forceRerender, sendAfter)

import SlamData.Effects (Slam)
import SlamData.Render.CSS as Rc
import SlamData.Render.Common (glyph, fadeWhen)

import Utils.DOM (focus)

type State =
  { inputs :: Map.Map Int String
  , elements :: Map.Map Int HTMLElement
  , blurred :: Maybe Int
  }

_inputs :: forall a r. LensP {inputs :: a|r} a
_inputs = lens _.inputs _{inputs = _}

_elements :: forall a r. LensP {elements :: a|r} a
_elements = lens _.elements _{elements = _}

_blurred :: forall a r. LensP {blurred :: a|r} a
_blurred = lens _.blurred _{blurred = _}

initialState :: State
initialState =
  { inputs: Map.empty
  , elements: Map.empty
  , blurred: Nothing
  }

data Query a
  = InputChanged Int String a
  | Remove Int a
  | Blurred Int a
  | Create Int a
  | Focused (Maybe Int) a
  | RememberEl Int (Maybe HTMLElement) a
  | GetValues (Array String -> a)

type UserShareDSL = H.ComponentDSL State Query Slam

comp :: H.Component State Query Slam
comp = H.component { render, eval }

render :: State -> H.ComponentHTML Query
render state =
  HH.form
    [ CP.nonSubmit
    , HP.classes [ Rc.userShareForm ]
    ]
    ((foldMap (uncurry showInput) $ Map.toList state.inputs)
     <> [ newInput newKey ]
    )
  where
  mbMaxKey :: Maybe Int
  mbMaxKey = F.maximum $ Map.keys state.inputs

  newKey :: Int
  newKey =
    fromMaybe zero $ add one mbMaxKey

  shouldFade :: Boolean
  shouldFade =
    mbMaxKey
      >>= flip Map.lookup state.inputs
      <#> eq ""
      # fromMaybe false

  showInput :: Int -> String -> Array (H.ComponentHTML Query)
  showInput inx val =
    [ HH.div
      [ HP.classes [ B.inputGroup ] ]
      [ HH.input
          [ HP.classes [ B.formControl ]
          , HP.value val
          , ARIA.label "User email or name"
          , HE.onValueInput (HE.input (InputChanged inx))
          , HE.onBlur (HE.input_ (Blurred inx))
          , HE.onFocus (HE.input_ (Focused $ Just inx))
          , HP.key $ show inx
          , HP.ref (H.action <<< RememberEl inx)
          ]
      , HH.span
          [ HP.classes [ B.inputGroupBtn ] ]
          [ HH.button
              [ HP.classes ([ B.btn ] <> (guard (val /= "") $> B.btnDefault))
              , HE.onClick (HE.input_ (Remove inx))
              , HP.buttonType HP.ButtonButton
              , HP.disabled (val == "")
              , ARIA.label "Clear user email or name"
              ]
              [ glyph B.glyphiconRemove ]
          ]
      ]
     ]

  newInput :: Int -> H.ComponentHTML Query
  newInput inx =
    HH.div
      [ HP.classes ([ B.inputGroup ] <> fadeWhen shouldFade) ]
      [ HH.input
          [ HP.classes [ B.formControl ]
          , ARIA.label "User email or name"
          , HE.onFocus (HE.input_ (Create inx))
          , HP.placeholder "Name or email of user"
          , HP.value ""
          , HP.key $ show inx
          , HP.ref (H.action <<< RememberEl inx)
          ]
      , HH.span
          [ HP.classes [ B.inputGroupBtn ] ]
          [ HH.button
              [ HP.classes [ B.btn ]
              , HP.disabled true
              , HP.buttonType HP.ButtonButton
              , ARIA.label "Clear user email or name"
              ]
              [ glyph B.glyphiconRemove ]
          ]
      ]


eval :: Natural Query UserShareDSL
eval (Create inx next) = do
  clearBlurred $ pure unit
  H.modify $ _inputs %~ Map.insert inx ""
  forceRerender
  focusInx inx
  pure next
eval (InputChanged inx val next) = H.modify (_inputs <<< ix inx .~ val) $> next
eval (Remove inx next) = forgetInx inx $> next
eval (Focused mbInx next) = do
  clearBlurred do
    forceRerender
    for_ mbInx focusInx
  pure next
eval (Blurred inx next) = do
  H.modify $ _blurred ?~ inx
  sendAfter (Milliseconds 200.0) $ H.action $ Focused Nothing
  pure next
eval (RememberEl inx el next) = H.modify (_elements %~ Map.alter (const el) inx) $> next
eval (GetValues continue) =
  H.gets _.inputs
    <#> Map.values
    <#> foldMap (\x -> guard (x /= "") $> x)
    <#> continue


forgetInx
  :: Int
  -> UserShareDSL Unit
forgetInx inx = do
  H.modify $ _inputs %~ Map.delete inx
  H.modify $ _elements %~ Map.delete inx

focusInx
  :: Int
  -> UserShareDSL Unit
focusInx inx =
  H.gets _.elements
    <#> Map.lookup inx
    >>= traverse_ (H.fromEff <<< focus)

clearBlurred
  :: UserShareDSL Unit
  -> UserShareDSL Unit
clearBlurred act =
  H.gets _.blurred >>= traverse_ \bix -> do
    val <-
      H.gets _.inputs
      <#> Map.lookup bix
      <#> fromMaybe ""
    when (val == "") $ forgetInx bix
    H.modify $ _blurred .~ Nothing
    act
