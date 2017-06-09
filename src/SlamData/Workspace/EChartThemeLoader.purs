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

module SlamData.Workspace.EChartThemeLoader (load) where

import SlamData.Prelude
import DOM.HTML.Location as Location
import DOM.HTML.Window as Window
import Data.URI as URI
import Network.HTTP.Affjax as Ajax
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (Aff)
import DOM (DOM)
import DOM.HTML (window)
import Data.Foldable (findMap)
import Data.Foreign (Foreign, toForeign, readString, tagOf)
import Data.Argonaut (toObject)
import ECharts.Theme (Theme(..), parseBuiltInTheme, builtInToTheme)
import Utils (hush)

load ∷ ∀ e. Aff (dom ∷ DOM, ajax ∷ Ajax.AJAX|e) (Maybe Theme)
load = runMaybeT $ do
  themeOrURL ← (MaybeT $ liftEff $ loadFromLocation) <|> (MaybeT $ liftEff $ loadFromForeign)
  traceAnyA "load"
  traceAnyA themeOrURL
  either pure (MaybeT ∘ fetchThemeFromURL) themeOrURL

loadFromLocation ∷ ∀ e. Eff (dom ∷ DOM|e) (Maybe (Either Theme Ajax.URL))
loadFromLocation = windowLocation <#> readThemeOrURIFromLocation

loadFromForeign ∷ ∀ e. Eff (dom ∷ DOM|e) (Maybe (Either Theme Ajax.URL))
loadFromForeign = windowEchartTheme <#> readThemeOrURLFromForeign

windowLocation ∷ ∀ e. Eff (dom ∷ DOM|e) String
windowLocation = window >>= Window.location >>= Location.href

foreign import windowEchartTheme ∷ ∀ e. Eff (dom ∷ DOM|e) Foreign

fetchThemeFromURL ∷  ∀ e. Ajax.URL → Aff (ajax ∷ Ajax.AJAX | e) (Maybe Theme)
fetchThemeFromURL uri = do
  res ← _.response <$> Ajax.get uri
  pure $ (FromObject ∘ toForeign) <$> (toObject res)

readThemeOrURIFromLocation ∷ String → Maybe (Either Theme Ajax.URL)
readThemeOrURIFromLocation location = case spy $ asURI location of
  Just (URI.URI _ _ (Just (URI.Query list)) _) → do
    traceAnyA list
    str ← findMap isECharThemeQueryParam list
    case asBuiltInTheme str of
      Just theme → pure $ Left theme
      Nothing → Right <$> asURL str
  _ → empty

isECharThemeQueryParam ∷ Tuple String (Maybe String) → Maybe String
isECharThemeQueryParam (Tuple "echart_theme" value) = value
isECharThemeQueryParam _ = Nothing

readThemeOrURLFromForeign ∷ Foreign → Maybe (Either Theme Ajax.URL)
readThemeOrURLFromForeign a = (builtInTheme <|> objectTheme <#> Left) <|> (themeURL <#> Right)
  where
    builtInTheme = asString a >>= asBuiltInTheme
    objectTheme = asObject a <#> FromObject
    themeURL = asString a >>= asURL

asURI ∷ String → Maybe URI.URI
asURI = hush ∘ URI.runParseURI

asURL ∷ String → Maybe Ajax.URL
asURL str = URI.printURI <$> asURI str

asBuiltInTheme ∷ String → Maybe Theme
asBuiltInTheme str = hush $ builtInToTheme <$> parseBuiltInTheme str

asObject ∷ Foreign → Maybe Foreign
asObject o = if tagOf o == "Object" then pure o else empty

asString ∷ Foreign → Maybe String
asString = hush ∘ runExcept ∘ readString
