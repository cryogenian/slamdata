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

module SlamData.FileSystem.Dialog.Mount.SQL2.Component.State where

import SlamData.Prelude

import Data.Array (filter)
import Data.Lens (Lens', lens)
import Data.String.Regex as Rx
import Data.String.Regex.Flags as RXF
import Data.StrMap as SM

type State =
  { initialQuery :: Maybe String
  , vars :: Array (Tuple String String)
  }

_initialQuery :: Lens' State (Maybe String)
_initialQuery = lens _.initialQuery (_ { initialQuery = _ })

_vars :: Lens' State (Array (Tuple String String))
_vars = lens _.vars (_ { vars = _ })

initialState :: State
initialState =
  { initialQuery: Nothing
  , vars: [emptyVar]
  }

stateFromViewInfo :: { query :: String, vars :: SM.StrMap String } -> State
stateFromViewInfo { query, vars } =
  processState
    { initialQuery: Just query
    , vars: SM.toUnfoldable vars
    }

emptyVar :: Tuple String String
emptyVar = Tuple "" ""

processState :: State -> State
processState s = s { vars = filter (not isEmptyVar) s.vars <> [emptyVar] }

isEmptyVar :: Tuple String String -> Boolean
isEmptyVar (Tuple name value) = Rx.test rxEmpty name && Rx.test rxEmpty value

rxEmpty :: Rx.Regex
rxEmpty = unsafePartial fromRight $ Rx.regex "^\\s*$" RXF.noFlags
