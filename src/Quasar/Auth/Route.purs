module Quasar.Auth.Route where

import Prelude

import Control.Bind (join)
import Control.Monad.Eff (Eff())
import Control.UI.Browser
  (alterLocation, modifyLocation, decodeURIComponent, encodeURIComponent, getHash)

import Data.Array as Arr
import Data.Maybe as M
import Data.Map as Map
import Data.String as Str
import Data.Foldable as F
import Data.List as L
import Data.Tuple as Tpl
import Data.String.Regex as Rgx

import DOM (DOM())

import Routing.Types (RoutePart(..))
import Routing.Parser (parse)
import Utils.Path

insertIntoString :: String -> String -> String -> String
insertIntoString key val hash =
  parse decodeURIComponent hash
  # insertIntoRouteParts key val
  # printRouteParts

insertIntoRouteParts :: String -> String -> L.List RoutePart -> L.List RoutePart
insertIntoRouteParts key val lst =
  L.snoc lst queryKeyVal
  where
  queryKeyVal =
    Query $ Map.fromList $ L.singleton $ Tpl.Tuple key val

printRouteParts :: L.List RoutePart -> String
printRouteParts lst =
  Str.joinWith "/"
  $ L.fromList
  $ map printRoutePart lst
  where
  printRoutePart :: RoutePart -> String
  printRoutePart (Path s) = s
  printRoutePart (Query m) =
    "?"
    <> (Str.joinWith "&"
        $ map (\(Tpl.Tuple k v) ->
                 k <> "=" <> encodeURIComponent v)
        $ L.fromList
        $ Map.toList m)

onHash
  :: (String -> M.Maybe String) -> String -> M.Maybe String
onHash hashFn old = do
  matchArr <- Rgx.match hashRgx old
  before <- join $ Arr.index matchArr 1
  after <- join $ Arr.index matchArr 2
  newHash <- hashFn $ Str.drop 1 after
  pure $ before <> "#" <> newHash
  where
  hashRgx = Rgx.regex "([^#]+)(.+)" Rgx.noFlags

extractToken
  :: String -> M.Maybe String
extractToken s =
  parse decodeURIComponent s
  # F.foldl foldFindToken M.Nothing
  where
  foldFindToken :: M.Maybe String -> RoutePart -> M.Maybe String
  foldFindToken (M.Just a) _ = M.Just a
  foldFindToken M.Nothing (Query m) =
    Map.lookup SlamData.Config.permissionsTokenField m
  foldFindToken acc _ = acc

permissionsToken
  :: forall e. Eff (dom :: DOM|e) (M.Maybe String)
permissionsToken =
  map extractToken getHash

preserveToken :: String -> String -> M.Maybe String
preserveToken old new = do
  token <- extractToken old
  Debug.Trace.traceAnyA token
  pure $ insertIntoString SlamData.Config.permissionsTokenField token new

setPreservingToken
  :: forall e. String -> Eff (dom :: DOM |e) Unit
setPreservingToken str =
  modifyLocation (\old -> M.fromMaybe str $ (onHash $ preserveToken old) str )

replacePreservingToken
  :: forall e. String -> Eff (dom :: DOM |e) Unit
replacePreservingToken str =
  alterLocation (\old -> M.fromMaybe str $ (onHash $ preserveToken old) str )
