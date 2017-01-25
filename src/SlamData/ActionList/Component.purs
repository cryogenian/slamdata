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
  , initialState
  , module SlamData.ActionList.Action
  , module SlamData.ActionList.Component.State
  , module SlamData.ActionList.Component.Query
  ) where

import SlamData.Prelude

import CSS as CSS

import Data.Array ((..))
import Data.Array as Array
import Data.Foldable as Foldable
import Data.Int as Int
import Data.String as String

import Halogen as H
import Halogen.HTML.CSS.Indexed as HCSS
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA

import Math as Math

import SlamData.Monad (Slam)
import SlamData.ActionList.Action as A
import SlamData.ActionList.Component.State (State)
import SlamData.ActionList.Component.Query (Query(..))

import Utils as Utils
import Utils.DOM (DOMRect)
import Utils.DOM as DOMUtils
import Utils.CSS as CSSUtils

import Unsafe.Coerce (unsafeCoerce)

hole ∷ ∀ a. a
hole = unsafeCoerce unit

type HTML a = H.ComponentHTML (Query a)
type DSL a = H.ComponentDSL (State a) (Query a) Slam

type ButtonConf a =
  { metrics ∷ ButtonMetrics
  , presentation ∷ Presentation
  , action ∷ Action a
  , lines ∷ Array String
  }

type ActionListConf a =
  { buttons ∷ Array (ButtonConf a)
  , leavesASpace ∷ Boolean
  }

actionListComp
  ∷ ∀ a. Eq a
  ⇒ (Array (Action a) → ActionListConf a)
  → H.Component (State a) (Query a) Slam
actionListComp conf =
  H.lifecycleComponent
    { render: render conf
    , initializer: Just $ H.action CalculateBoundingRect
    , finalizer: Nothing
    , eval
    }

defaultConf ∷ ∀ a. Eq a ⇒ (Array (Action a)) → ActionListConf a
defaultConf as = hole

comp ∷ ∀ a. Eq a ⇒ H.Component (State a) (Query a) Slam
comp = actionListComp defaultConf

render ∷ ∀ a. ActionListConf a → State a → HTML a
render mkConf state =
  HH.div
    [ HP.class_ $ HH.className "sd-action-list" ]
    [ HH.ul
        [ HP.ref $ H.action ∘ SetBoundingElement ]
        $ renderButtons (String.toLower state.filterString)
        $ mkConf state.actions
    ]

renderButtons
  ∷ ∀ a
  . String
  → ActionListConf a
  → Array (HTML a)
renderButtons filterString conf =
  if conf.leaveASpace
    then realButtons <> [ renderSpaceFillerButton metrics ]
    else realButtons
  where
  realButtons ∷ Array (HTML a)
  realButtons =
    renderButton filterString <$> conf.buttons

renderSpaceFillerButton ∷ ∀ a. ButtonMetrics → HTML a
renderSpaceFillerButton metrics =
  HH.li
    [ HCSS.style
        $ CSS.width (CSS.px $ firefoxify metrics.dimensions.width)
        *> CSS.height (CSS.px $ firefoxify metrics.dimensions.height)
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
  → ButtonConf a
  → HTML a
renderButton filterString { presentation, metrics, action, lines } =
  HH.li
    [ HCSS.style
        $ CSS.width (CSS.px $ firefoxify metrics.dimensions.width)
        *> CSS.height (CSS.px $ firefoxify metrics.dimensions.height)
    ]
    [ HH.button
        attrs
        $ case presentation of
            IconOnly →
              [ renderIcon ]
            TextOnly →
              [ renderName ]
            IconAndText →
              [ renderIcon, renderName ]
    ]
  where
  renderIcon ∷ HTML a
  renderIcon =
    HH.img
      [ HP.src $ pluckActionIconSrc action
      , HCSS.style
          $ CSS.width (CSS.px metrics.iconDimensions.width)
          *> CSS.height (CSS.px metrics.iconDimensions.height)
          *> CSS.marginBottom (CSS.px metrics.iconMarginPx)
          -- Stops icon only presentations from being cut off in short wide
          -- buttons.
          *> (if (isIconOnly presentation)
                then
                  CSS.position CSS.absolute
                    *> CSS.left (CSS.px metrics.iconOnlyLeftPx)
                    *> CSS.top (CSS.px metrics.iconOnlyTopPx)
                else
                  CSS.position CSS.relative)
      ]

  renderName ∷ HTML a
  renderName =
    HH.p
      [ HCSS.style
          $ CSS.fontSize (CSS.px $ fontSizePx)
          *> CSSUtils.lineHeight (show lineHeightPx <> "px")
      ]
      $ Array.intercalate
          [ HH.br_ ]
          $ Array.singleton ∘ HH.text <$> lines

  enabled ∷ Boolean
  enabled =
    case action of
      GoBack →
        true
      _ →
        Foldable.any
          (String.contains (String.Pattern filterString) ∘ String.toLower)
          (searchFilters action)

  attrs =
    [ HP.title $ pluckActionDescription action
    , HP.disabled $ (not enabled) || (isDisabled action)
    , HP.buttonType HP.ButtonButton
    , ARIA.label $ pluckActionDescription action
    , HP.classes classes
    , HE.onClick (HE.input_ $ Selected action)
    , HCSS.style $ CSS.position CSS.relative
    ]

  classes ∷ Array HH.ClassName
  classes =
    if isHighlighted action && enabled
      then
        [ HH.className "sd-button" ]
      else
        [ HH.className "sd-button"
        , HH.className "sd-button-warning"
        ]

updateActions ∷ ∀ a. Eq a ⇒ Array (Action a) → State a → State a
updateActions newActions state =
  case activeDrill of
    Nothing →
      state
        { actions = newActions }
    Just drill →
      state
        { previousActions = newActions
        , actions = fromMaybe [] $ pluckDrillActions =<< newActiveDrill
        }
  where
  activeDrill ∷ Maybe (Action a)
  activeDrill =
    Foldable.find
      (maybe false (eq state.actions) ∘ pluckDrillActions)
      state.previousActions

  newActiveDrill =
    Foldable.find (eq activeDrill ∘ Just) newActions

getBoundingDOMRect ∷ ∀ a. DSL a (Maybe DOMRect)
getBoundingDOMRect =
  traverse (H.fromEff ∘ DOMUtils.getOffsetClientRect)
    =<< H.gets _.boundingElement

eval ∷ ∀ a. Eq a ⇒ Query a ~> DSL a
eval =
  case _ of
    UpdateFilter str next → do
      H.modify _{ filterString = str }
      pure next
    Selected action next → do
      st ← H.get
      case action of
        Do _ → pure unit
        Drill {children} →
          H.modify _
            { actions = GoBack `Array.cons` children
            , previousActions = st.actions
            }
        GoBack →
          H.modify _
            { actions = st.previousActions
            , previousActions = [ ]
            }
      pure next
    UpdateActions actions next →
      H.modify (updateActions actions)
        $> next
    CalculateBoundingRect next →
      calculateBoundingRect $> next
    SetBoundingRect dimensions next →
      H.modify _{ boundingDimensions = maybeNotZero dimensions } $> next
    GetBoundingRect continue →
      continue <$> H.gets _.boundingDimensions
    SetBoundingElement element next →
      H.modify _{ boundingElement = element } $> next

calculateBoundingRect ∷ ∀ a. DSL a Unit
calculateBoundingRect =
  H.modify
    ∘ flip _{ boundingDimensions = _ }
    ∘ flip bind maybeNotZero
    ∘ map domRectToDimensions
    =<< getBoundingDOMRect
