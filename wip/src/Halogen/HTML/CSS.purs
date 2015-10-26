{-
Copyright 2015 SlamData, Inc.

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

module Halogen.HTML.CSS
  ( Styles(..)
  , runStyles

  , style
  , stylesheet
  ) where

import Prelude

import Data.Array (mapMaybe)
import Data.Either (Either(), either)
import Data.List (fromList, toList)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))

import qualified Data.StrMap as SM

import Css.Property (Key(), Value())
import Css.Render (render, renderedSheet, collect)
import Css.Stylesheet (Css(), Rule(..), runS)

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Properties as P
import qualified Halogen.HTML.Core as H

-- | A newtype for CSS styles
newtype Styles = Styles (SM.StrMap String)

-- | Unpack CSS styles
runStyles :: Styles -> SM.StrMap String
runStyles (Styles m) = m

instance isPropStyles :: H.IsProp Styles where
  toPropString _ _ (Styles m) = joinWith "; " $ (\(Tuple key value) -> key <> ": " <> value) <$> fromList (SM.toList m)

-- | Render a set of rules as an inline style.
-- |
-- | For example:
-- |
-- | ```purescript
-- | H.div [ Css.style do color red
-- |                      display block ]
-- |       [ ... ]
-- | ```
style :: forall i. Css -> H.Prop i
style css =
  H.prop (H.propName "style") (Just $ H.attrName "style")
  $ Styles $  rules $ runS css
  where
  rules :: Array Rule -> SM.StrMap String
  rules rs = SM.fromList (toList properties)
    where
    properties :: Array (Tuple String String)
    properties = mapMaybe property rs >>= collect >>> rights

  property :: Rule -> Maybe (Tuple (Key Unit) Value)
  property (Property k v) = Just (Tuple k v)
  property _              = Nothing

  rights :: forall a b. Array (Either a b) -> Array b
  rights = mapMaybe (either (const Nothing) Just)

-- | Render a set of rules as a `style` element.
stylesheet :: forall p i. Css -> H.HTML p i
stylesheet css = H.style [ P.type_ "text/css" ] [ H.text content ]
  where
  content = fromMaybe "" $ renderedSheet $ render css
