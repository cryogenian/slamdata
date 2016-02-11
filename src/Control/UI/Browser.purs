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

module Control.UI.Browser
  ( locationObject
  , replaceLocation
  , decodeURIComponent
  , encodeURIComponent
  , setLocation
  , locationString
  , select
  , newTab
  , clearValue
  , reload
  , setTitle
  , permissionsToken
  ) where

import Prelude

import Control.Monad.Eff (Eff())

import Data.Maybe as M
import Data.Map as Map
import Data.String as Str
import Data.Foldable as F
import Data.List as L
import Data.Tuple as Tpl

import DOM (DOM())
import DOM.HTML.Types (HTMLElement(), Location())
import DOM.HTML (window)
import DOM.HTML.Location as Location
import DOM.HTML.Window as Window

import Routing.Types (RoutePart(..))
import Routing.Parser (parse)

locationObject :: forall e. Eff (dom :: DOM | e) Location
locationObject =
  window
    >>= Window.location

replaceLocation :: forall e. String -> Eff (dom :: DOM | e) Unit
replaceLocation str = do
  locationObject
    >>= Location.replace str

setLocation :: forall e. String -> Eff (dom :: DOM | e) Unit
setLocation str =
  locationObject
    >>= Location.assign str

reload :: forall e. Eff (dom :: DOM | e) Unit
reload =
  locationObject
    >>= Location.reload

foreign import locationString :: forall e. Eff (dom :: DOM | e) String
foreign import select :: forall e. HTMLElement -> Eff (dom :: DOM | e) Unit
foreign import newTab :: forall e. String -> Eff (dom :: DOM | e) Unit
foreign import clearValue :: forall e. HTMLElement -> Eff (dom :: DOM | e) Unit
foreign import setTitle :: forall e. String -> Eff (dom :: DOM | e) Unit

foreign import decodeURIComponent :: String -> String
foreign import encodeURIComponent :: String -> String

insertIntoString :: String -> String -> String -> String
insertIntoString key val hash =
  Str.drop 1 hash
  # parse decodeURIComponent
  # insertIntoRouteParts key val
  # printRouteParts

insertIntoRouteParts :: String -> String -> L.List RoutePart -> L.List RoutePart
insertIntoRouteParts key val lst =
  M.fromMaybe (L.snoc lst queryKeyVal) do
    lstQueryIndex <- L.findLastIndex findFn lst
    L.alterAt lstQueryIndex insertFn lst
  where
  findFn (Path _) = false
  findFn (Query _) = true

  insertFn :: RoutePart -> M.Maybe RoutePart
  insertFn (Path _) = M.Nothing
  insertFn (Query m) = M.Just $ Query $ Map.insert key val m

  queryKeyVal =
    Query $ Map.fromList $ L.singleton $ Tpl.Tuple key val


printRouteParts :: L.List RoutePart -> String
printRouteParts lst = Str.joinWith "/" $ L.fromList $ map printRoutePart lst
  where
  printRoutePart :: RoutePart -> String
  printRoutePart (Path s) = s
  printRoutePart (Query m) =
    "?"
    <> (Str.joinWith "&"
        $ map (\(Tpl.Tuple k v) -> k <> "=" <> v)
        $ L.fromList
        $ Map.toList m)


routeParts
  :: forall e. Eff (dom :: DOM|e) (L.List RoutePart)
routeParts =
  locationObject
    >>= Location.hash
    <#> Str.drop 1
    <#> parse decodeURIComponent

permissionsToken
  :: forall e. Eff (dom :: DOM|e) (M.Maybe String)
permissionsToken =
  routeParts
    <#> F.foldl foldFindToken M.Nothing
  where
  foldFindToken :: M.Maybe String -> RoutePart -> M.Maybe String
  foldFindToken (M.Just a) _ = M.Just a
  foldFindToken M.Nothing (Query m) =
    Debug.Trace.spy $
    Map.lookup SlamData.Config.permissionsTokenField m
  foldFindToken acc _ = acc
