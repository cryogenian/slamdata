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

module SlamData.ActionList.Component where

import SlamData.Prelude

import CSS as CSS

import Data.Array ((..))
import Data.Array as Array
import Data.Foldable as Foldable
import Data.Int as Int
import Data.Lens (Lens', lens, (.~))
import Data.String as String

import DOM.HTML.Types (HTMLElement)

import Halogen as H
import Halogen.HTML.CSS.Indexed as HCSS
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA

import Math as Math

import SlamData.Monad (Slam)
import SlamData.Render.Common as RC

import Utils as Utils
import Utils.DOM (DOMRect)
import Utils.DOM as DOMUtils
import Utils.CSS as CSSUtils

data Query a b
  = Selected (ActionInternal a) b
  | UpdateFilter String b
  | UpdateActions (Array (Action a)) b
  | CalculateBoundingRect b
  | GetBoundingRect (Maybe Dimensions → b)
  | SetBoundingRect Dimensions b
  | SetBoundingElement (Maybe HTMLElement) b

type State a =
  { actions ∷ Array (ActionInternal a)
  , previousActions ∷ Array (ActionInternal a)
  , filterString ∷ String
  , boundingElement ∷ Maybe HTMLElement
  , boundingDimensions ∷ Maybe Dimensions
  }

type HTML a = H.ComponentHTML (Query a)
type DSL a = H.ComponentDSL (State a) (Query a) Slam

-- TODO: Add Pixels newtype
newtype ActionIconSrc = ActionIconSrc String
newtype ActionName = ActionName String
newtype ActionNameWord = ActionNameWord { word ∷ String, widthPx ∷ Number }
newtype ActionNameLine = ActionNameLine { line ∷ String, widthPx ∷ Number }
newtype ActionDescription = ActionDescription String
newtype ActionHighlighted = ActionHighlighted Boolean
newtype FilterInputDescription = FilterInputDescription String

data Action a
  = Do ActionName ActionIconSrc ActionDescription ActionHighlighted a
  | Drill ActionName ActionIconSrc ActionDescription (Array (Action a))

data ActionInternal a
  = DoInternal (Array ActionNameWord) ActionIconSrc ActionDescription ActionHighlighted a
  | DrillInternal (Array ActionNameWord) ActionIconSrc ActionDescription (Array (ActionInternal a))
  | GoBackInternal

data Presentation
  = IconOnly
  | TextOnly
  | IconAndText

type Dimensions = { width ∷ Number, height ∷ Number }

type ButtonMetrics =
  { dimensions ∷ Dimensions
  , iconDimensions ∷ Dimensions
  , iconMarginPx ∷ Number
  , iconOnlyLeftPx ∷ Number
  , iconOnlyTopPx ∷ Number
  }

derive newtype instance eqActionIconSrc :: Eq ActionIconSrc
derive newtype instance eqActionDescription :: Eq ActionDescription
derive newtype instance eqActionHighlighted :: Eq ActionHighlighted
derive instance eqActionNameWord ∷ Eq ActionNameWord
derive instance eqActionInternal ∷ Eq a ⇒ Eq (ActionInternal a)

fontSizePx ∷ Number
fontSizePx =
  12.0

lineHeightPx ∷ Number
lineHeightPx =
  13.0

iconSizeRatio ∷ Number
iconSizeRatio =
  0.3

-- Buttons are rendered with mystery centering and padding.
-- This can cause text to overflow.
buttonPaddingHighEstimate ∷ Number
buttonPaddingHighEstimate =
  0.2

isIconOnly ∷ Presentation → Boolean
isIconOnly =
  case _ of
    IconOnly → true
    _ → false

wordify ∷ ActionName → Array ActionNameWord
wordify (ActionName s) =
  ActionNameWord
    ∘ (\word → { word, widthPx: textWidth word })
    <$> Utils.words s

printActionNameWord ∷ ActionNameWord → String
printActionNameWord (ActionNameWord { word }) =
  word

printActionNameLine ∷ ActionNameLine → String
printActionNameLine (ActionNameLine { line }) =
  line

printActionNameWords ∷ Array ActionNameWord → String
printActionNameWords =
  String.joinWith " " ∘ map printActionNameWord

toActionInternal ∷ ∀ a. Action a → ActionInternal a
toActionInternal =
  case _ of
    Do actionName actionIconSrc actionDescription actionHighlighted a →
      DoInternal
        (wordify actionName)
        actionIconSrc
        actionDescription
        actionHighlighted
        a
    Drill actionName actionIconSrc actionDescription xs →
      DrillInternal
        (wordify actionName)
        actionIconSrc
        actionDescription
        (toActionInternal <$> xs)

_actions ∷ ∀ a r. Lens' { actions ∷ a |r } a
_actions =
  lens _.actions (_ { actions = _ })

_previousActions ∷ ∀ a r. Lens' { previousActions ∷ a | r} a
_previousActions =
  lens _.previousActions (_ { previousActions = _ })

_filterString ∷ ∀ a r. Lens' { filterString ∷ a | r } a
_filterString =
  lens _.filterString (_ { filterString = _ })

_boundingElement ∷ ∀ a r. Lens' { boundingElement ∷ a | r } a
_boundingElement =
  lens _.boundingElement (_ { boundingElement = _ })

_boundingDimensions ∷ ∀ a r. Lens' { boundingDimensions ∷ a | r } a
_boundingDimensions =
  lens _.boundingDimensions (_ { boundingDimensions = _ })

isDo ∷ ∀ a. ActionInternal a → Boolean
isDo =
  case _ of
    DoInternal _ _ _ _ _ →
      true
    _ →
      false

isDrill ∷ ∀ a. ActionInternal a → Boolean
isDrill =
  case _ of
    DrillInternal _ _ _ _ →
      true
    _ →
      false

isHighlighted ∷ ∀ a. ActionInternal a → Boolean
isHighlighted =
  case _ of
    DoInternal _ _ _ (ActionHighlighted highlighted) _ →
      highlighted
    DrillInternal _ _ _ actions →
      Foldable.any isHighlighted actions
    GoBackInternal → true

searchFilters ∷ ∀ a. ActionInternal a → Array String
searchFilters =
  case _ of
    DoInternal words _ _ _ _ →
      [ printActionNameWords words ]
    DrillInternal words _ _ actions →
      [ printActionNameWords words ] ⊕ Array.concat (map searchFilters actions)
    GoBackInternal →
      [ "go back" ]

pluckActionDescription ∷ ∀ a. ActionInternal a → String
pluckActionDescription =
  case _ of
    DoInternal _ _ (ActionDescription s) _ _ →
      s
    DrillInternal _ _ (ActionDescription s) _ →
      s
    GoBackInternal →
      "Go back"

pluckActionIconSrc ∷ ∀ a. ActionInternal a → String
pluckActionIconSrc =
  case _ of
    DoInternal _ (ActionIconSrc s) _ _ _ →
      s
    DrillInternal _ (ActionIconSrc s) _ _ →
      s
    GoBackInternal →
      "/img/go-back.svg"

actionNameWords ∷ ∀ a. ActionInternal a → Array ActionNameWord
actionNameWords =
  case _ of
    DoInternal xs _ _ _ _ →
      xs
    DrillInternal xs _ _ _ →
      xs
    GoBackInternal →
      wordify $ ActionName "Go back"

initialState ∷ ∀ a. Array (Action a) → State a
initialState actions =
  { actions: toActionInternal <$> actions
  , previousActions: [ ]
  , filterString: ""
  , boundingElement: Nothing
  , boundingDimensions: Nothing
  }

comp ∷ ∀ a. Eq a ⇒ FilterInputDescription → H.Component (State a) (Query a) Slam
comp filterInputDescription =
  H.lifecycleComponent
    { render: render filterInputDescription
    , initializer: Just $ H.action CalculateBoundingRect
    , finalizer: Nothing
    , eval
    }

render ∷ ∀ a. FilterInputDescription → State a → HTML a
render (FilterInputDescription filterInputDescription) state =
  HH.div
    [ HP.class_ $ HH.className "sd-action-list" ]
    [ HH.form_
        [ HH.div_
            [ HH.div
                [ HP.class_ (HH.className "sd-action-filter-icon") ]
                [ RC.searchFieldIcon ]
            , HH.input
                [ HP.value state.filterString
                , HE.onValueInput (HE.input (\s → UpdateFilter s))
                , ARIA.label filterInputDescription
                , HP.placeholder filterInputDescription
                ]
            , HH.button
                [ HP.buttonType HP.ButtonButton
                , HE.onClick (HE.input_ (UpdateFilter ""))
                , HP.enabled (state.filterString /= "")
                ]
                [ RC.clearFieldIcon "Clear filter" ]
            ]
        ]
    , HH.ul
        [ HP.ref $ H.action ∘ SetBoundingElement ]
        (maybe
           []
           (renderButtons (String.toLower state.filterString) state.actions)
           (actionSize
              (Array.length state.actions)
              =<< state.boundingDimensions))
    ]
  where
  actionSize
    ∷ Int
    → Dimensions
    → Maybe { dimensions ∷ Dimensions, leavesASpace ∷ Boolean }
  actionSize i boundingDimensions = do
    firstTry ← mostSquareFittingRectangle i boundingDimensions
    if firstTry.height ≡ boundingDimensions.height
      then do
        secondTry ← mostSquareFittingRectangle (i + 1) boundingDimensions
        if secondTry.height ≡ boundingDimensions.height
           then pure { dimensions: firstTry, leavesASpace: false }
           else pure { dimensions: secondTry, leavesASpace: true }
        else pure { dimensions: firstTry, leavesASpace: false }

renderButtons
  ∷ ∀ a
  . String
  → Array (ActionInternal a)
  → { dimensions ∷ Dimensions, leavesASpace ∷ Boolean }
  → Array (HTML a)
renderButtons filterString actions buttonDimensions =
  if buttonDimensions.leavesASpace
    then realButtons <> [ renderSpaceFillerButton metrics ]
    else realButtons
  where
  realButtons ∷ Array (HTML a)
  realButtons =
    renderButton filterString presentation metrics <$> actionsWithLines

  actionsWithLines ∷ Array { action ∷ ActionInternal a, lines ∷ Array String }
  actionsWithLines =
    toActionWithLines (buttonDimensions.dimensions.width * 0.95) <$> actions

  toActionWithLines
    ∷ Number
    → ActionInternal a
    → { action ∷ ActionInternal a, lines ∷ Array String }
  toActionWithLines widthPx action =
    { action
    , lines: printActionNameLine <$> (calculateLines widthPx $ actionNameWords action)
    }

  iconDimensions ∷ Dimensions
  iconDimensions =
    { width: buttonDimensions.dimensions.width * iconSizeRatio
    , height: buttonDimensions.dimensions.height * iconSizeRatio
    }

  metrics ∷ ButtonMetrics
  metrics =
    { dimensions:
        buttonDimensions.dimensions
    , iconDimensions:
        iconDimensions
    , iconMarginPx:
        buttonDimensions.dimensions.height * 0.05
    , iconOnlyLeftPx:
        (buttonDimensions.dimensions.width / 2.0)
          - (iconDimensions.width / 2.0)
    , iconOnlyTopPx:
        (buttonDimensions.dimensions.height / 2.0)
          - (iconDimensions.height / 2.0) - 1.0
    }

  maxNumberOfLines ∷ Int
  maxNumberOfLines =
    fromMaybe 0 $ Foldable.maximum $ Array.length ∘ _.lines <$> actionsWithLines

  maxTextHeightPx ∷ Number
  maxTextHeightPx =
    Int.toNumber maxNumberOfLines * fontSizePx

  buttonPaddingEstimatePx ∷ Number
  buttonPaddingEstimatePx =
    buttonDimensions.dimensions.height * buttonPaddingHighEstimate

  textDoesNotFitWithIcon ∷ Boolean
  textDoesNotFitWithIcon =
    maxTextHeightPx
      + iconDimensions.height
      + metrics.iconMarginPx
      + buttonPaddingEstimatePx
      > buttonDimensions.dimensions.height

  textDoesNotFitOnItsOwn ∷ Boolean
  textDoesNotFitOnItsOwn =
    maxTextHeightPx > buttonDimensions.dimensions.height
      ∨ buttonDimensions.dimensions.width < 40.0

  -- Allows text which fits vertically but not horizontally.
  -- Styling truncates overflowing lines with ellipses.
  --
  -- E.g. where there is only room for two lines:
  -- "Show Chart" would be presented without an icon as
  --
  -- Sh...
  -- Ch...
  --
  -- But "Build pie chart" would be presented with only an icon.
  --
  -- The reasoning behind this is that presentation should be
  -- unform per action list but that the entire list shouldn't be
  -- reduced to only icons just because "Troubleshoot" would be
  -- truncated to "Troublesh...".
  presentation ∷ Presentation
  presentation =
    if textDoesNotFitOnItsOwn ∨ maxNumberOfLines ≡ 0
      then
        IconOnly
      else
        if textDoesNotFitWithIcon
          then
            TextOnly
          else
            IconAndText

decimalCrop ∷ Int → Number → Number
decimalCrop i n =
  (Math.floor $ n * multiplier) / multiplier
  where
  multiplier = Math.pow 10.0 $ Int.toNumber i

-- Firefox doesn't seem to be able to handle pixel metrics with decimal
-- precisons higher than one. Without applying this function actionlists
-- of certain widths render with empty spaces and overflowing actions.
firefoxify ∷ Number → Number
firefoxify n =
  if Utils.isFirefox
     then decimalCrop 1 n
     else n

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
  → Presentation
  → ButtonMetrics
  → { action ∷ ActionInternal a, lines ∷ Array String }
  → HTML a
renderButton filterString presentation metrics { action, lines } =
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
      GoBackInternal →
        true
      _ →
        Foldable.any
          (String.contains (String.Pattern filterString) ∘ String.toLower)
          (searchFilters action)

  attrs =
    [ HP.title $ pluckActionDescription action
    , HP.disabled $ not enabled
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

factors ∷ Int → Array Int
factors n = do
  factor ← 1 .. n
  guard $ n `mod` factor ≡ 0
  pure factor

updateActions ∷ ∀ a. Eq a ⇒ Array (ActionInternal a) → State a → State a
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
  activeDrill ∷ Maybe (ActionInternal a)
  activeDrill =
    Foldable.find
      (maybe false (eq state.actions) ∘ pluckDrillActions)
      state.previousActions

  newActiveDrill =
    Foldable.find (eq activeDrill ∘ Just) newActions

  pluckDrillActions =
    case _ of
      DrillInternal _ _ _ xs → Just xs
      _ → Nothing

domRectToDimensions ∷ DOMRect → Dimensions
domRectToDimensions domRect =
  { width: domRect.width, height: domRect.height }

getBoundingDOMRect ∷ ∀ a. DSL a (Maybe DOMRect)
getBoundingDOMRect =
  traverse (H.fromEff ∘ DOMUtils.getOffsetClientRect)
    =<< H.gets _.boundingElement

eval ∷ ∀ a. Eq a ⇒ Query a ~> DSL a
eval =
  case _ of
    UpdateFilter str next →
      H.modify (_filterString .~ str) $> next
    Selected action next → do
      st ← H.get
      case action of
        DoInternal _ _ _ _ _ → pure unit
        DrillInternal _ _ _ actions →
          H.modify
            $ (_actions .~ (GoBackInternal `Array.cons` actions))
            ∘ (_previousActions .~ st.actions)
        GoBackInternal →
          H.modify
            $ (_actions .~ st.previousActions)
            ∘ (_previousActions .~ [ ])
      pure next
    UpdateActions actions next →
      H.modify (updateActions $ toActionInternal <$> actions)
        $> next
    CalculateBoundingRect next →
      calculateBoundingRect $> next
    SetBoundingRect dimensions next →
      H.modify (_boundingDimensions .~ Just dimensions) $> next
    GetBoundingRect continue →
      continue <$> H.gets _.boundingDimensions
    SetBoundingElement element next →
      H.modify (_boundingElement .~ element) $> next

calculateBoundingRect ∷ ∀ a. DSL a Unit
calculateBoundingRect =
  H.modify
    ∘ (_boundingDimensions .~ _)
    ∘ map domRectToDimensions
    =<< getBoundingDOMRect

floor ∷ Dimensions → Dimensions
floor dimensions =
  { width: Math.floor dimensions.width
  , height: Math.floor dimensions.height
}

calculateLines ∷ Number → Array ActionNameWord → Array ActionNameLine
calculateLines width words =
  foldl go [] words
  where
  go ∷ Array ActionNameLine → ActionNameWord → Array ActionNameLine
  go acc (ActionNameWord { word, widthPx: wordWidthPx }) =
    case Array.uncons acc of
      Nothing →
        [ ActionNameLine { line: word, widthPx: wordWidthPx } ]
      Just { head: ActionNameLine { line, widthPx: lineWidthPx }, tail } →
        if (lineWidthPx + spaceWidth + wordWidthPx) <= width
          then
            Array.snoc
              tail
              $ ActionNameLine
                  { line: line <> " " <> word
                  , widthPx: lineWidthPx + spaceWidth + wordWidthPx
                  }
          else
            Array.snoc
              acc
              $ ActionNameLine { line: word, widthPx: wordWidthPx }

spaceWidth ∷ Number
spaceWidth =
  textWidth " "

textWidth ∷ String → Number
textWidth =
  flip
    DOMUtils.getTextWidthPure
    $ "normal " <> show fontSizePx <> "px Ubuntu"

mostSquareFittingRectangle ∷ Int → Dimensions → Maybe Dimensions
mostSquareFittingRectangle i boundingDimensions =
  Foldable.maximumBy
    (\x y → karat x `compare` karat y)
    solutions
  where
  solutions ∷ Array Dimensions
  solutions =
    solution <$> factors i

  goldenRatio ∷ Number
  goldenRatio =
    1.61803398875

  karat ∷ Dimensions → Number
  karat dimensions =
    -(Math.abs $ goldenRatio - (dimensions.width / dimensions.height))

  solution ∷ Int → Dimensions
  solution factor =
    { width: boundingDimensions.width / (Int.toNumber $ numberOfRows factor)
    , height: boundingDimensions.height / (Int.toNumber $ numberOfColumns factor)
    }

  numberOfRows ∷ Int → Int
  numberOfRows factor =
    if boundingDimensions.width > boundingDimensions.height
       then factor
       else i / factor

  numberOfColumns ∷ Int → Int
  numberOfColumns factor =
    if boundingDimensions.width > boundingDimensions.height
       then i / factor
       else factor
