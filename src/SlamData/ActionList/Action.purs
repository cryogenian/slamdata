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

module SlamData.ActionList.Action where

import SlamData.Prelude

import Data.Array as A
import Data.Int as Int
import Data.Foldable as F
import Data.NonEmpty ((:|))
import Data.NonEmpty as NE
import Data.String as String

import Halogen.HTML as HH

import Math as Math

import Utils as Utils
import Utils.DOM as DOMUtils
import Utils.DOM (DOMRect)

type NameWord =
  { word ∷ String
  , width ∷ Number
  }

type NameLine =
  { line ∷ String
  , width ∷ Number
  }

type Dimensions = { width ∷ Number, height ∷ Number }

type ActionSizes =
  { dimensions ∷ Array Dimensions
  , leavesASpace ∷ Boolean
  }

type ButtonMetrics =
  { dimensions ∷ Dimensions
  , iconDimensions ∷ Dimensions
  , iconMarginPx ∷ Number
  , iconOnlyLeftPx ∷ Number
  , iconOnlyTopPx ∷ Number
  }

data Presentation
  = IconOnly
  | TextOnly
  | IconAndText


type InputAndActionR r =
  { iconSrc ∷ String
  , description ∷ String
  | r }

type DoMixin a =
  ( action ∷ a, highlighted ∷ Boolean, disabled ∷ Boolean )

type DrillMixin a =
  ( children ∷ Array (Action a) )

type ActionR r =
  InputAndActionR (words ∷ Array NameWord |r)

type DoR a =
  ActionR (DoMixin a)

type DrillR a =
  ActionR (DrillMixin a)

type MkR r =
  InputAndActionR (name ∷ String |r)

type MkDoR a =
  MkR (DoMixin a)

type MkDrillR a =
  MkR (DrillMixin a)

data Action a
  = Do (DoR a)
  | Drill (DrillR a)
  | GoBack

type ButtonConf a =
  { metrics ∷ ButtonMetrics
  , presentation ∷ Presentation
  , action ∷ Action a
  , lines ∷ Array String
  }

type ActionListConf a =
  { buttons ∷ Array (ButtonConf a)
  , leavesASpace ∷ Boolean
  , classes ∷ Array HH.ClassName
  }


mkDo ∷ ∀ a. MkDoR a → Action a
mkDo {name, iconSrc, description, highlighted, disabled, action} =
  Do { words: wordify name
             , iconSrc
             , description
             , highlighted
             , disabled
             , action
             }

mkDrill ∷ ∀ a. MkDrillR a → Action a
mkDrill {name, iconSrc, description, children} =
  Drill { words: wordify name
                , iconSrc
                , description
                , children
                }

eqActionR ∷ ∀ r rr. ActionR r → ActionR rr → Boolean
eqActionR ar1 ar2 =
  (F.and $ A.zipWith eqNameWord ar1.words ar2.words)
  ∧ ar1.iconSrc ≡ ar2.iconSrc
  ∧ ar1.description ≡ ar2.description

eqNameWord ∷ NameWord → NameWord → Boolean
eqNameWord nw1 nw2 =
  nw1.word ≡ nw2.word
  ∧ nw1.width ≡ nw2.width

instance eqAction ∷ Eq a ⇒ Eq (Action a) where
  eq GoBack GoBack =
    true
  eq (Do di1) (Do di2) =
    eqActionR di1 di2
    ∧ di1.action ≡ di2.action
    ∧ di1.highlighted ≡ di2.highlighted
    ∧ di1.disabled ≡ di2.disabled
  eq (Drill di1) (Drill di2) =
    eqActionR di1 di2
    ∧ di1.children ≡ di2.children
  eq _ _ =
    false


wordify ∷ String → Array NameWord
wordify =
  Utils.words ⋙ map (\word → { word, width: textWidth word })

textWidth ∷ String → Number
textWidth =
  flip
    DOMUtils.getTextWidthPure
    $ "normal " <> show fontSizePx <> "px Ubuntu"

fontSizePx ∷ Number
fontSizePx =
  12.0

isIconOnly ∷ Presentation → Boolean
isIconOnly = case _ of
  IconOnly → true
  _ → false

printActionNameWords ∷ Array NameWord → String
printActionNameWords =
  String.joinWith " " ∘ map _.word

isDo ∷ ∀ a. Action a → Boolean
isDo = case _ of
  Do _ → true
  _ → false

isDrill ∷ ∀ a. Action a → Boolean
isDrill = case _ of
  Drill _ → true
  _ → false

isHighlighted ∷ ∀ a. Action a → Boolean
isHighlighted = case _ of
  Do {highlighted} →
    highlighted
  Drill {children} →
    F.any isHighlighted children
  GoBack → true

isDisabled ∷ ∀ a. Action a → Boolean
isDisabled = case _ of
  Do {disabled} →
    disabled
  Drill {children} →
    F.all isDisabled children
  GoBack → false

searchFilters ∷ ∀ a. Action a → Array String
searchFilters = case _ of
  Do {words} →
    [ printActionNameWords words ]
  Drill {words, children} →
    [ printActionNameWords words ] ⊕ A.concat (map searchFilters children)
  GoBack →
    [ "go back" ]

pluckActionDescription ∷ ∀ a. Action a → String
pluckActionDescription = case _ of
  Do {description} →
    description
  Drill {description} →
    description
  GoBack →
    "Go back"

pluckActionIconSrc ∷ ∀ a. Action a → String
pluckActionIconSrc = case _ of
  Do {iconSrc} →
    iconSrc
  Drill {iconSrc} →
    iconSrc
  GoBack →
    "/img/go-back.svg"

actionNameWords ∷ ∀ a. Action a → Array NameWord
actionNameWords = case _ of
  Do {words} →
    words
  Drill {words} →
    words
  GoBack →
    wordify "Go back"

calculateLines ∷ Number → Array NameWord → Array NameLine
calculateLines totalWidth words =
  foldl go [] words
  where
  go ∷ Array NameLine → NameWord → Array NameLine
  go acc { word, width: wordWidthPx } = case A.uncons acc of
    Nothing →
      [ { line: word, width: wordWidthPx } ]
    Just { head: { line, width: lineWidthPx }, tail }
      | (lineWidthPx + spaceWidth + wordWidthPx) <= totalWidth →
        A.snoc tail
          { line: line ⊕ " " ⊕ word
          , width: lineWidthPx + spaceWidth + wordWidthPx
          }
      | otherwise →
        A.snoc acc
          { line: word, width: wordWidthPx }

spaceWidth ∷ Number
spaceWidth =
  textWidth " "

mostSquareFittingRectangle ∷ Int → Dimensions → Dimensions
mostSquareFittingRectangle i boundingDimensions = case tailMax of
  Just m | compareFn hd m ≡ LT → m
  _ → hd
  where
  hd ∷ Dimensions
  hd = NE.head solutions

  tailMax ∷ Maybe Dimensions
  tailMax =
    F.maximumBy compareFn $ NE.tail solutions

  compareFn x y =
    karat x `compare` karat y

  solutions ∷ NE.NonEmpty Array Dimensions
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



floor ∷ Dimensions → Dimensions
floor dimensions =
  { width: Math.floor dimensions.width
  , height: Math.floor dimensions.height
}

domRectToDimensions ∷ DOMRect → Dimensions
domRectToDimensions domRect =
  { width: domRect.width
  , height: domRect.height
  }

pluckDrillActions ∷ ∀ a. Action a → Maybe (Array (Action a))
pluckDrillActions = case _ of
  Drill {children} → Just children
  _ → Nothing

maybeNotZero ∷ Dimensions → Maybe Dimensions
maybeNotZero dimensions
  | dimensions.width ≡ 0.0 ∨ dimensions.height ≡ 0.0 = Nothing
  | otherwise = Just dimensions

factors ∷ Int → NE.NonEmpty Array Int
factors n = 1 :| do
  factor ← 2 A... n
  guard $ n `mod` factor ≡ 0
  pure factor

-- Firefox doesn't seem to be able to handle pixel metrics with decimal
-- precisons higher than one. Without applying this function actionlists
-- of certain widths render with empty spaces and overflowing actions.
firefoxify ∷ Number → Number
firefoxify n =
  if Utils.isFirefox
     then decimalCrop 1 n
     else n


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

decimalCrop ∷ Int → Number → Number
decimalCrop i n =
  (Math.floor $ n * multiplier) / multiplier
  where
  multiplier = Math.pow 10.0 $ Int.toNumber i

defaultConf ∷ ∀ a. Eq a ⇒ Dimensions → Array (Action a) → ActionListConf a
defaultConf boundingDimensions as =
  let
    len = A.length as

    firstTry = mostSquareFittingRectangle len boundingDimensions

    actionSize
      | firstTry.height ≠ boundingDimensions.height =
          { dimensions: firstTry, leavesASpace: false }
      | otherwise =
          let secondTry = mostSquareFittingRectangle (len + 1) boundingDimensions
          in if secondTry.height ≡ boundingDimensions.height
             then { dimensions: firstTry, leavesASpace: false }
             else { dimensions: secondTry, leavesASpace: true }

    dimensions =
      { width: actionSize.dimensions.width
      , height: actionSize.dimensions.height
      }

    iconDimensions =
      { width: dimensions.width * iconSizeRatio
      , height: dimensions.height * iconSizeRatio
      }

    iconOnlyLeftPx =
      dimensions.width / 2.0 - iconDimensions.width / 2.0
    iconOnlyTopPx =
      dimensions.height / 2.0 - iconDimensions.height / 2.0
    iconMarginPx =
      dimensions.height * 0.05

    metrics =
      { dimensions
      , iconDimensions
      , iconMarginPx
      , iconOnlyLeftPx
      , iconOnlyTopPx
      }


    mkButtonConf action =
      { action
      , metrics
      , lines: map _.line $ calculateLines dimensions.width $ actionNameWords action
      , presentation: IconAndText
      }

    buttonConfs = map mkButtonConf as

    maxNumberOfLines =
      fromMaybe zero $ F.maximum $ map (A.length ∘ _.lines) buttonConfs

    maxTextHeightPx =
      Int.toNumber maxNumberOfLines * fontSizePx

    buttonPaddingEstimatePx ∷ Number
    buttonPaddingEstimatePx =
      dimensions.height * buttonPaddingHighEstimate

    textDoesNotFitWithIcon ∷ Boolean
    textDoesNotFitWithIcon =
      maxTextHeightPx
        + iconDimensions.height
        + iconMarginPx
        + buttonPaddingEstimatePx
        > dimensions.height

    textDoesNotFitOnItsOwn ∷ Boolean
    textDoesNotFitOnItsOwn =
      maxTextHeightPx > dimensions.height
      ∨ dimensions.width < 40.0


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
    presentation
      | textDoesNotFitOnItsOwn ∨ maxNumberOfLines ≡ 0 = IconOnly
      | textDoesNotFitWithIcon = TextOnly
      | otherwise = IconAndText

    buttons = map _{ presentation = presentation } buttonConfs

  in
    { buttons
    , leavesASpace: actionSize.leavesASpace
    , classes: [ HH.ClassName "with-filter" ]
    }
