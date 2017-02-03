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

module SlamData.ActionList.Component
  ( comp
  , module A
  , module ST
  , module Q
  ) where

import SlamData.Prelude

import CSS as CSS

import Data.Array as Array
import Data.Foldable as F
import Data.String as String
import Data.Int as Int
import Data.NonEmpty ((:|))
import Data.NonEmpty as NE

import Halogen as H
import Halogen.HTML.CSS.Indexed as HCSS
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA

import SlamData.Monad (Slam)
import SlamData.ActionList.Action as A
import SlamData.ActionList.Component.State as ST
import SlamData.ActionList.Component.Query as Q

import Utils.DOM (DOMRect)
import Utils.DOM as DOMUtils
import Utils.CSS as CSSUtils

type HTML a = H.ComponentHTML (Q.Query a)
type DSL a = H.ComponentDSL (ST.State a) (Q.Query a) Slam

type MkConf a =
  A.Dimensions → Array (A.Action a) → A.ActionListConf a

actionListComp
  ∷ ∀ a. Eq a
  ⇒ MkConf a
  → H.Component (ST.State a) (Q.Query a) Slam
actionListComp mkConf =
  H.lifecycleComponent
    { render: render mkConf
    , initializer: Just $ H.action Q.CalculateBoundingRect
    , finalizer: Nothing
    , eval
    }

comp ∷ ∀ a. Eq a ⇒ H.Component (ST.State a) (Q.Query a) Slam
comp = actionListComp A.defaultConf

render ∷ ∀ a. ST.State a → HTML a
render state =
  HH.div
    [ HP.classes
      $ [ HH.className "sd-action-list" ]
      ⊕ conf.classes
    ]
    [ HH.ul
        [ HP.ref $ H.action ∘ Q.SetBoundingElement ]
        $ (realButtons buttonConfs)
        ⊕ (renderSpaceFillerButton dimensions <$ guard leavesASpace)
    ]
  where
  filterString ∷ String
  filterString =
    String.toLower state.filterString

  actionCount ∷ Int
  actionCount = Array.length st.actions

  boundingDimensions ∷ A.Dimensions
  boundingDimensions = fromMaybe { width: 0.0, height: 0.0 } state.boundingDimensions

  -- factors for integer from 2
  factors ∷ Int → Array Int
  factors = do
    factor ← 2 Array... actionCount
    guard $ n `mod` factor ≡ 0
    pure factor

  albumOriented ∷ Boolean
  albumOriented =
    boundingDimensions.width > boundingDimensions.height

  numberOfRows ∷ Int → Int → Int
  numberOfRows i factor
    | albumOriented = factor
    | otherwise = i / factor

  numberOfColumns ∷ Int → Int → Int
  numberOfColumns i factor
    | albumOriented = actionCount / factor
    | otherwise = factor

  solution ∷ Int → Int → Dimensions
  solution i factor =
    { width: boundingDimensions.width / (Int.toNumber $ numberOfRows i factor)
    , height: boundingDimensions.height / (Int.toNumber $ numberOfColumns i factor)
    }

  -- solutions from 2
  solutions ∷ Int → Array A.Dimensions
  solutions i =
    map solution $ factors i

  goldenRatio ∷ Number
  goldenRatio =
    1.61803398875

  karat ∷ A.Dimensions → Number
  karat dimensions =
    -(Math.abs $ goldenRatio - (dimensions.width / dimensions.height))

  compareFn ∷ A.Dimensions → A.Dimensions → Ordering
  compareFn x y =
    karat x `compare` karat y

  tailMax ∷ Int → Maybe Dimensions
  tailMax i =
    F.maximumBy compareFn $ solutions i

  -- one is always a factor for integer. this is one row solution
  hd ∷ Dimensions
  hd = solution 1

  fittingRectangle ∷ Int → A.Dimensions
  fittingRectangle i =
    case tailMax i of
      Just m | compareFn hd m ≡ LT →  m
      _ → hd

  firstTry ∷ A.Dimensions
  firstTry = fittingRectangle actionCount

  secondTry ∷ A.Dimensions
  secondTry = fittingRectangle actionCount

  needASpaceAndButtonDims ∷ A.Dimensions × Boolean
  needASpaceAndButtonDims
    | firstTry.height ≠ boundingDimensions.height =
        firstTry × false
    | secondTry.height ≡ boundingDimensions.height =
        firstTry × false
    | otherwise =
        secondTry × true

  dimensions ∷ A.Dimensions
  dimensions = fst needASpaceAndButtonDims

  leavesASpace ∷ Boolean
  leavesASpace = snd needASpaceAndButtonDims

  buttonConfs ∷ Array (String × A.Dimensions × A.Action a)
  buttonConfs = map (\a → filterString × dimensions × a) st.actions

iconSizeRatio ∷ Number
iconSizeRatio = 0.3

buttonPaddingHighEstimate ∷ Number
buttonPaddingHighEstimate = 0.2

lineHeightPx ∷ Number
lineHeightPx =
  13.0

textWidth ∷ String → Number
textWidth =
  flip
    DOMUtils.getTextWidthPure
    $ "normal " <> show fontSizePx <> "px Ubuntu"

fontSizePx ∷ Number
fontSizePx =
  12.0

realButtons ∷ ∀ a. Array (String × A.Dimensions × A.Action a) → Array HTML
realButtons tpls =
  map mkButtonConf tpls
  where
  dimensions ∷ A.Dimensions
  dimensions = maybe { width: 0.0, height: 0.0 } (\(_ × a × _) → a) $ Array.head tpls

  maxNumberOfLines =
    fromMaybe zero $ F.maximum $ map (A.length ∘ _.lines) buttonConfs

  maxTextHeightPx =
    Int.toNumber maxNumberOfLines * fontSizePx


  buttonPaddingEstimatePx =
    dimensions.height

  rawButtonConfs ∷ Array (ButtonConf a)
  rawButtonConfs = map mkButtonConf tpls

  mkButtonConf ∷ String × A.Dimensions × A.Action → ButtonConf a
  mkButtonConf (filterString × dimensions × action) =
    let
      iconDimensions =
        { width: dimensions.width * iconSizeRatio
        , height: dimensions.height * iconSizeRatio
        }

      iconOnlyLeftPx =
        (dimensions.width - iconDimensions.width) / 2.0

      iconOnlyTop =
        (dimensions.height - iconDimensions.height) / 2.0

      iconMarginPx =
        dimensions.height * 0.05

      lines =
        map _.line $ calculateLines dimensions.width $ A.actionNameWords action

      presentation =
        IconAndText

    in { dimensions
       , action
       , filterString
       , presentation
       , lines
       , iconOnlyTopPx
       , iconOnlyLeftPx
       , iconMarginPx
       }

  map (\(f × d × a) → renderButton f d a) confs


renderSpaceFillerButton ∷ ∀ a. A.Dimensions → HTML a
renderSpaceFillerButton dimensions =
  HH.li
    [ HCSS.style do
         CSS.width (CSS.px $ A.firefoxify dimensions.width)
         CSS.height (CSS.px $ A.firefoxify dimensions.height)
    ]
    [ HH.button
        [ HP.classes
            [ HH.className "sd-button"
            , HH.className "sd-button-warning"
            ]
        , HP.disabled true
        , HP.buttonType HP.ButtonButton
        ]
        []
    ]

renderButton
  ∷ ∀ a
  . String
  → A.Dimensions
  → A.Action a
  → HTML a
renderButton filterString dimensions action = -- { presentation, metrics, action, lines } =
  HH.li
    [ HCSS.style do
        CSS.width (CSS.px $ A.firefoxify dimensions.width)
        CSS.height (CSS.px $ A.firefoxify dimensions.height)
    ]
    [ HH.button attrs
        case presentation of
          A.IconOnly →
            [ renderIcon ]
          A.TextOnly →
            [ renderName ]
          A.IconAndText →
            [ renderIcon, renderName ]
    ]
  where
  renderIcon ∷ HTML a
  renderIcon =
    HH.img
      [ HP.src $ A.pluckActionIconSrc action
      , HCSS.style do
          CSS.width (CSS.px metrics.iconDimensions.width)
          CSS.height (CSS.px metrics.iconDimensions.height)
          CSS.marginBottom (CSS.px metrics.iconMarginPx)
          -- Stops icon only presentations from being cut off in short wide
          -- buttons.
          if A.isIconOnly presentation
            then do
              CSS.position CSS.absolute
              CSS.left (CSS.px metrics.iconOnlyLeftPx)
              CSS.top (CSS.px metrics.iconOnlyTopPx)
            else
              CSS.position CSS.relative)
      ]

  renderName ∷ HTML a
  renderName =
    HH.p
      [ HCSS.style do
          CSS.fontSize (CSS.px $ A.fontSizePx)
          CSSUtils.lineHeight (show A.lineHeightPx <> "px")
      ]
      $ Array.intercalate
          [ HH.br_ ]
          $ Array.singleton ∘ HH.text <$> lines

  enabled ∷ Boolean
  enabled =
    case action of
      A.GoBack →
        true
      _ →
        F.any
          (String.contains (String.Pattern filterString) ∘ String.toLower)
          (A.searchFilters action)

  attrs =
    [ HP.title $ A.pluckActionDescription action
    , HP.disabled $ (not enabled) || (A.isDisabled action)
    , HP.buttonType HP.ButtonButton
    , ARIA.label $ A.pluckActionDescription action
    , HP.classes classes
    , HE.onClick $ HE.input_ $ Q.Selected action
    , HCSS.style $ CSS.position CSS.relative
    ]

  classes ∷ Array HH.ClassName
  classes =
    if A.isHighlighted action ∧ enabled
      then
        [ HH.className "sd-button" ]
      else
        [ HH.className "sd-button"
        , HH.className "sd-button-warning"
        ]

updateActions ∷ ∀ a. Eq a ⇒ Array (A.Action a) → ST.State a → ST.State a
updateActions newActions state =
  case activeDrill of
    Nothing →
      state
        { actions = newActions }
    Just drill →
      state
        { previousActions = newActions
        , actions = fromMaybe [] $ A.pluckDrillActions =<< newActiveDrill
        }
  where
  activeDrill ∷ Maybe (A.Action a)
  activeDrill =
    F.find
      (maybe false (eq state.actions) ∘ A.pluckDrillActions)
      state.previousActions

  newActiveDrill ∷ Maybe (A.Action a)
  newActiveDrill =
    F.find (eq activeDrill ∘ Just) newActions

getBoundingDOMRect ∷ ∀ a. DSL a (Maybe DOMRect)
getBoundingDOMRect =
  traverse (H.fromEff ∘ DOMUtils.getOffsetClientRect)
    =<< H.gets _.boundingElement

eval ∷ ∀ a. Eq a ⇒ Q.Query a ~> DSL a
eval =
  case _ of
    Q.UpdateFilter str next → do
      H.modify _{ filterString = str }
      pure next
    Q.Selected action next → do
      st ← H.get
      case action of
        A.Do _ → pure unit
        A.Drill {children} →
          H.modify _
            { actions = A.GoBack `Array.cons` children
            , previousActions = st.actions
            }
        A.GoBack →
          H.modify _
            { actions = st.previousActions
            , previousActions = [ ]
            }
      pure next
    Q.UpdateActions actions next →
      H.modify (updateActions actions)
        $> next
    Q.CalculateBoundingRect next → do
      H.modify
        ∘ flip _{ boundingDimensions = _ }
        ∘ flip bind A.maybeNotZero
        ∘ map A.domRectToDimensions
        =<< getBoundingDOMRect
      pure next
    Q.SetBoundingRect dimensions next →
      H.modify _{ boundingDimensions = A.maybeNotZero dimensions } $> next
    Q.GetBoundingRect continue →
      continue <$> H.gets _.boundingDimensions
    Q.SetBoundingElement element next →
      H.modify _{ boundingElement = element } $> next
