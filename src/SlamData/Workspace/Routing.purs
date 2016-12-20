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

module SlamData.Workspace.Routing
  ( routing
  , Routes(..)
  , mkWorkspaceHash
  , mkWorkspaceURL
  , varMapsForURL
  , encodeVarMaps
  , getPath
  , getURLVarMaps
  ) where

import SlamData.Prelude

import Data.Argonaut ((~>), (:=))
import Data.Argonaut as J
import Data.Foldable as F
import Data.Json.Extended as EJSON
import Data.List ((:))
import Data.List as L
import Data.Map as Map
import Data.Maybe as M
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as P
import Data.String as Str
import Data.String.Regex as R
import Data.String.Regex.Flags as RXF
import Data.StrMap as SM

import Global (encodeURIComponent)

import Routing.Match (Match)
import Routing.Match (eitherMatch, list) as Match
import Routing.Match.Class (lit, str, param) as Match

import SlamData.Config as Config
import SlamData.Workspace.AccessType (AccessType(..))
import SlamData.Workspace.Action as WA
import SlamData.Workspace.Card.Port.VarMap as Port
import SlamData.Workspace.Deck.DeckId as D

import Utils.Path as UP

data Routes
  = WorkspaceRoute
      UP.DirPath
      (Maybe D.DeckId)
      WA.Action
      (Map.Map D.DeckId Port.URLVarMap)

getPath ∷ Routes → UP.DirPath
getPath (WorkspaceRoute p _ _ _) = p

getURLVarMaps ∷ Routes → Map.Map D.DeckId Port.URLVarMap
getURLVarMaps (WorkspaceRoute _ _ _ vm) = vm

routing ∷ Match Routes
routing
  = WorkspaceRoute <$> workspace <*> deckId <*> action <*> optionalVarMap

  where
  optionalVarMap ∷ Match (Map.Map D.DeckId Port.URLVarMap)
  optionalVarMap = varMap <|> pure Map.empty

  varMap ∷ Match (Map.Map D.DeckId Port.URLVarMap)
  varMap = Match.eitherMatch $ decodeVarMaps <$> Match.param "vars"

  oneSlash ∷ Match Unit
  oneSlash = Match.lit ""

  explored ∷ Match UP.FilePath
  explored = Match.eitherMatch $ mkResource <$> Match.list Match.str

  mkResource ∷ L.List String → Either String UP.FilePath
  mkResource parts =
    case L.last parts of
      Just filename | filename /= "" →
        let dirParts = unsafePartial M.fromJust (L.init parts)
            filePart = P.file filename
            path = foldr (\part acc → P.dir part </> acc) filePart dirParts
        in Right $ P.rootDir </> path
      _ → Left "Expected non-empty explore path"

  workspace ∷ Match UP.DirPath
  workspace = workspaceFromParts <$> partsAndName

  workspaceFromParts ∷ Tuple (L.List String) String → UP.DirPath
  workspaceFromParts (Tuple ps nm) =
    foldl (</>) P.rootDir (map P.dir ps) </> P.dir nm

  partsAndName ∷ Match (Tuple (L.List String) String)
  partsAndName = Tuple <$> (oneSlash *> Match.list notName) <*> name

  name ∷ Match String
  name = Match.eitherMatch $ map workspaceName Match.str

  notName ∷ Match String
  notName = Match.eitherMatch $ map pathPart Match.str

  workspaceName ∷ String → Either String String
  workspaceName input
    | checkExtension input = Right input
    | otherwise = Left input

  pathPart ∷ String → Either String String
  pathPart input
    | input == "" || checkExtension input = Left "incorrect path part"
    | otherwise = Right input

  extensionRegex ∷ R.Regex
  extensionRegex =
    unsafePartial fromRight $
      R.regex ("\\." <> Config.workspaceExtension <> "$") RXF.noFlags

  checkExtension ∷ String → Boolean
  checkExtension = R.test extensionRegex

  deckId ∷ Match (Maybe D.DeckId)
  deckId
      = Match.eitherMatch (map (map Just ∘ D.fromString') Match.str)
    <|> pure Nothing

  action ∷ Match WA.Action
  action
      = (WA.Exploring <$> (Match.lit "exploring" *> explored))
    <|> (Match.eitherMatch $ map WA.parseAction Match.str)
    <|> pure (WA.Load ReadOnly)

-- TODO: it would be nice if `purescript-routing` had a way to render a route
-- from a matcher, so that we could do away with the following brittle functions.

mkWorkspaceURL
  ∷ UP.DirPath    -- workspace path
  → WA.Action     -- workspace action
  → String
mkWorkspaceURL path action =
  Config.workspaceUrl
    <> mkWorkspaceHash path action Map.empty

mkWorkspaceHash
  ∷ UP.DirPath -- workspace path
  → WA.Action -- workspace action
  → Map.Map D.DeckId Port.URLVarMap -- varmaps introduced by variables cards in the workspace
  → String
mkWorkspaceHash path action varMap =
  "#"
    <> UP.encodeURIPath (P.printPath path)
    <> WA.printAction action
    <> maybe "" ("/" <> _) (renderVarMapQueryString varMap)

varMapsForURL ∷ Map.Map D.DeckId Port.VarMap → Map.Map D.DeckId Port.URLVarMap
varMapsForURL = map (map go)
  where
  go (Port.Literal ej) = goEJson ej
  go (Port.SetLiteral as) = "(" <> F.intercalate "," (goEJson <$> as) <> ")"
  go (Port.QueryExpr q) =
    -- | This is not entirely legit as it will strip backticks from SQL²
    -- | expressions as well as identifiers, as we have no information about
    -- | the field type here... -gb
    fromMaybe q $ Str.stripPrefix (Str.Pattern "`") =<< Str.stripSuffix (Str.Pattern "`") q

  goEJson ej = case EJSON.unroll ej of
    EJSON.String str → str
    EJSON.Timestamp str → str
    EJSON.Date str → str
    EJSON.Time str → str
    EJSON.Interval str → str
    EJSON.ObjectId str → str
    _ → EJSON.renderEJson ej

decodeVarMaps ∷ String → Either String (Map.Map D.DeckId Port.URLVarMap)
decodeVarMaps = J.jsonParser >=> J.decodeJson >=> \obj →
  Map.fromFoldable <$> L.foldM go L.Nil (SM.toList obj)
  where
  go
    ∷ L.List (D.DeckId × Port.URLVarMap)
    → String × J.Json
    → Either String (L.List (D.DeckId × Port.URLVarMap))
  go acc (key × json) = do
    deckId ← D.fromString' key
    varMap ← J.decodeJson json
    pure $ (deckId × varMap) : acc

renderVarMapQueryString ∷ Map.Map D.DeckId Port.URLVarMap → Maybe String
renderVarMapQueryString varMaps
  | F.all SM.isEmpty varMaps = Nothing
  | otherwise =
      let json = encodeVarMaps varMaps
      in Just $ "?vars=" <> encodeURIComponent (show json)

encodeVarMaps ∷ Map.Map D.DeckId Port.URLVarMap → J.Json
encodeVarMaps = foldl go J.jsonEmptyObject ∘ Map.toList
  where
  go ∷ J.Json → D.DeckId × Port.URLVarMap → J.Json
  go acc (deckId × varMap)
    = D.toString deckId := encodeVarMap varMap
    ~> acc

encodeVarMap ∷ Port.URLVarMap → J.Json
encodeVarMap = foldl go J.jsonEmptyObject ∘ SM.toList
  where
  go ∷ J.Json → String × String → J.Json
  go acc (k × v) = k := v ~> acc
