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

module Utils.Completions where

import Prelude

import Ace.Halogen.Component (AceEffects)
import Ace.Types (Completion)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Data.Argonaut as J
import Data.Array as Arr
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Path.Pathy as P
import Data.String as S
import Data.Tuple (Tuple(..))
import DOM (DOM)
import SlamData.FileSystem.Resource as R
import SlamData.LocalStorage.Class (retrieve, persist)
import SlamData.LocalStorage.Keys as LSK
import Utils.Path as PU


pathCompletions :: forall e. Aff (AceEffects e) (Array Completion)
pathCompletions = do
  paths <- retrieve J.decodeJson LSK.autoCompletePathsKey <#> either (const []) id
  pure $ paths <#> S.drop 1 >>> mkCompletion "path" mkCaption
  where
  mkCaption :: String -> Maybe String
  mkCaption val =
    pure $  "/" <> if S.length val > 30
                   then S.take 28 val <> "…"
                   else val


mkCompletion :: String -> (String -> Maybe String) -> String -> Completion
mkCompletion meta f val =
  { value: val
  , score: 200.0
  , meta: meta
  , caption: f val
  }


memoizeCompletionStrs
  :: forall e. PU.DirPath -> Array R.Resource -> Aff (dom :: DOM, avar :: AVAR | e) Unit
memoizeCompletionStrs dir arr = do
  alreadyMemoized <- retrieve J.decodeJson LSK.autoCompletePathsKey <#> either (const []) id
  persist J.encodeJson LSK.autoCompletePathsKey
    $ Arr.sort $ newSiblings <> filterSiblings alreadyMemoized
  where
  parentPath :: String
  parentPath = P.printPath dir

  filterSiblings :: Array String -> Array String
  filterSiblings =
    Arr.filter
    $ fromMaybe true
    <<< map (S.contains (S.Pattern "/"))
    <<< S.stripPrefix (S.Pattern parentPath)

  newSiblings :: Array String
  newSiblings = Arr.catMaybes $ map (resToMbPath) arr

  resToMbPath :: R.Resource -> Maybe String
  resToMbPath (R.File p) = case P.peel p of
    Just (Tuple _ (Right (P.FileName f))) | f /= ".folder" -> pure $ P.printPath p
    _ -> Nothing
  resToMbPath _ = Nothing
