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

module SlamData.Workspace.Card.Variables.Eval where

import SlamData.Prelude

import Data.Map as Map
import Data.StrMap as SM

import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Variables.Model (Model)
import SlamData.Workspace.Deck.DeckId (DeckId)
import SlamData.Workspace.FormBuilder.Item.Model (defaultValueToVarMapValue)

eval
  ∷ DeckId
  → Map.Map DeckId Port.URLVarMap
  → Model
  → Port.VarMap
eval deckId urlVarMaps model =
  foldl alg SM.empty model.items
  where
  alg =
    flip \{ name, fieldType, defaultValue } →
      maybe id (SM.insert name) $ defaultValueToVarMapValue fieldType
        =<< (SM.lookup name =<< Map.lookup deckId urlVarMaps)
        <|> defaultValue
