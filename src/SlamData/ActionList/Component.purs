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
  ( actionListComp
  , component
  , MkConf
  , module A
  , module ST
  , module Q
  , module M
  ) where

import SlamData.Prelude

import CSS as CSS

import Data.Array as Array
import Data.Foldable as Foldable
import Data.String as String

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA

import RectanglePacking (Dimensions, maybeNotZero, domRectToDimensions)

import SlamData.Monad (Slam)
import SlamData.ActionList.Action as A
import SlamData.ActionList.Component.State as ST
import SlamData.ActionList.Component.Query as Q
import SlamData.ActionList.Component.Message as M

import Utils.DOM (DOMRect)
import Utils.DOM as DOMUtils
import Utils.CSS as CSSUtils

type HTML a = H.ComponentHTML (Q.Query a)
type DSL a = H.ComponentDSL (ST.State a) (Q.Query a) (M.Message a) Slam

type MkConf a =
  Dimensions → Array (A.Action a) → A.ActionListConf a

actionListComp
  ∷ ∀ a. Eq a
  ⇒ MkConf a
  → Array (A.Action a)
  → H.Component HH.HTML (Q.Query a) Unit (M.Message a) Slam
actionListComp mkConf actions =
  H.lifecycleComponent
    { initialState: const (ST.initialState actions)
    , render: render mkConf
    , initializer: Just $ H.action Q.CalculateBoundingRect
    , finalizer: Nothing
    , eval
    , receiver: const Nothing
    }

elementRef ∷ H.RefLabel
elementRef = H.RefLabel "bounding-element"

component ∷ ∀ a. Eq a ⇒ H.Component HH.HTML (Q.Query a) Unit (M.Message a) Slam
component = actionListComp A.defaultConf []

render ∷ ∀ a. MkConf a → ST.State a → HTML a
render mkConf state =
  HH.div
    [ HP.classes
      $ [ HH.ClassName "sd-action-list" ]
      ⊕ conf.classes
    ]
    [ HH.ul
        [ HP.ref elementRef ]
        $ renderButtons (String.toLower state.filterString) conf

    ]
  where
  conf =
    mkConf (fromMaybe { width: 0.0, height: 0.0 } state.boundingDimensions) state.actions

renderButtons
  ∷ ∀ a
  . String
  → A.ActionListConf a
  → Array (HTML a)
renderButtons filterString conf =
  realButtons ⊕ foldMap (pure ∘ renderSpaceFillerButton) metrics
  where
  metrics ∷ Maybe A.ButtonMetrics
  metrics = do
    guard conf.leavesASpace
    map _.metrics $ Array.head conf.buttons

  realButtons ∷ Array (HTML a)
  realButtons =
    renderButton filterString <$> conf.buttons

renderSpaceFillerButton ∷ ∀ a. A.ButtonMetrics → HTML a
renderSpaceFillerButton metrics =
  HH.li
    [ HCSS.style
        $ CSS.width (CSS.px $ A.firefoxify metrics.dimensions.width)
        *> CSS.height (CSS.px $ A.firefoxify metrics.dimensions.height)
    ]
    [ HH.button
        [ HP.classes
            [ HH.ClassName "sd-button"
            , HH.ClassName "sd-button-warning"
            ]
        , HP.disabled true
        , HP.type_ HP.ButtonButton
        ]
        []
    ]

renderButton
  ∷ ∀ a
  . String
  → A.ButtonConf a
  → HTML a
renderButton filterString { presentation, metrics, action, lines } =
  HH.li
    [ HCSS.style
        $ CSS.width (CSS.px $ A.firefoxify metrics.dimensions.width)
        *> CSS.height (CSS.px $ A.firefoxify metrics.dimensions.height)
    ]
    [ HH.button
        attrs
        $ case presentation of
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
      , HCSS.style
          $ CSS.width (CSS.px metrics.iconDimensions.width)
          *> CSS.height (CSS.px metrics.iconDimensions.height)
          *> CSS.marginBottom (CSS.px metrics.iconMarginPx)
          -- Stops icon only presentations from being cut off in short wide
          -- buttons.
          *> (if (A.isIconOnly presentation)
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
          $ CSS.fontSize (CSS.px $ A.fontSizePx)
          *> CSSUtils.lineHeight (show A.lineHeightPx <> "px")
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
        Foldable.any
          (String.contains (String.Pattern filterString) ∘ String.toLower)
          (A.searchFilters action)

  attrs =
    [ HP.title $ A.pluckActionDescription action
    , HP.disabled $ (not enabled) || (A.isDisabled action)
    , HP.type_ HP.ButtonButton
    , ARIA.label $ A.pluckActionDescription action
    , HP.classes classes
    , HE.onClick $ HE.input_ $ Q.HandleSelected action
    , HCSS.style $ CSS.position CSS.relative
    ]

  classes ∷ Array HH.ClassName
  classes =
    if A.isHighlighted action && enabled
      then
        [ HH.ClassName "sd-button" ]
      else
        [ HH.ClassName "sd-button"
        , HH.ClassName "sd-button-warning"
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
    Foldable.find
      (maybe false (eq state.actions) ∘ A.pluckDrillActions)
      state.previousActions

  newActiveDrill ∷ Maybe (A.Action a)
  newActiveDrill =
    Foldable.find (eq activeDrill ∘ Just) newActions

getBoundingDOMRect ∷ ∀ a. DSL a (Maybe DOMRect)
getBoundingDOMRect =
  traverse (H.liftEff ∘ DOMUtils.getOffsetClientRect)
    =<< H.getHTMLElementRef elementRef

eval ∷ ∀ a. Eq a ⇒ Q.Query a ~> DSL a
eval =
  case _ of
    Q.UpdateFilter str next → do
      H.modify _{ filterString = str }
      pure next
    Q.HandleSelected action next → do
      st ← H.get
      case action of
        A.Do a → do
          H.raise (M.Selected a.action)
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
        ∘ flip bind maybeNotZero
        ∘ map domRectToDimensions
        =<< getBoundingDOMRect
      pure next
    Q.SetBoundingRect dimensions next →
      H.modify _{ boundingDimensions = maybeNotZero dimensions } $> next
    Q.GetBoundingRect continue →
      continue <$> H.gets _.boundingDimensions
