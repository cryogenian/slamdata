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

module SlamData.Workspace.Eval.Traverse
  ( TraverseDeck(..)
  , TraverseCard
  , unfoldTree
  , childPointers
  , getVarMaps
  , getSharingInput
  ) where

import SlamData.Prelude

import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set

import SlamData.Workspace.Card.Model as CM
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Variables.Eval as Variables
import SlamData.Workspace.Deck.Dialog.Share.Model (SharingInput)
import SlamData.Workspace.Eval.Card as Card
import SlamData.Workspace.Eval.Deck as Deck

import Utils (censor)
import Utils.Path (DirPath)

data TraverseDeck = TraverseDeck
  { deckId ∷ Deck.Id
  , cards ∷ List TraverseCard
  }

type TraverseCard =
  { coord ∷ Card.Coord
  , model ∷ Card.AnyCardModel
  , children ∷ List TraverseDeck
  , sources ∷ Set Card.AdditionalSource
  }

unfoldTree
  ∷ Map Card.Coord Card.Cell
  → Map Deck.Id Deck.Cell
  → Deck.Id
  → Maybe TraverseDeck
unfoldTree cards decks deckId =
  go ∘ _.model <$> Map.lookup deckId decks
  where
    go deck =
      TraverseDeck
        { deckId
        , cards: List.catMaybes (unfoldCard <$> List.fromFoldable (Deck.cardCoords deckId deck))
        }

    unfoldCard ∷ Card.Coord → Maybe TraverseCard
    unfoldCard coord =
      Map.lookup coord cards <#> \{ value: { model, sources } } →
        { coord
        , model: model.model
        , children: List.catMaybes $ unfoldTree cards decks <$> childPointers model
        , sources
        }

childPointers ∷ Card.Model → List Deck.Id
childPointers = case _ of
  { cardId, model: CM.Draftboard model } →
    List.catMaybes (List.fromFoldable model.layout)
  _ → mempty

getVarMaps ∷ TraverseDeck → Map Deck.Id Port.VarMap
getVarMaps = Map.fromFoldable ∘ goDeck
  where
    goDeck (TraverseDeck { cards }) =
      foldMap goCard cards

    goCard ∷ TraverseCard → List (Deck.Id × Port.VarMap)
    goCard { coord: deckId × _, model: CM.Variables vars } =
      pure $ deckId × Variables.buildVarMap deckId Map.empty vars
    goCard { children } = foldMap goDeck children

getSharingInput ∷ DirPath → TraverseDeck → SharingInput
getSharingInput path (TraverseDeck root) =
  { workspacePath: path
  , deckId: root.deckId
  , decks: List.mapMaybe censor resources
  , caches: List.mapMaybe isCache resources
  , sources: List.mapMaybe isSource resources
  }
  where
    resources =
      List.fromFoldable
        (foldMap goCard root.cards)

    goCard { coord: deckId × _, sources, children } =
      foldMap goDeck children <> Set.map Left sources <>
      if deckId ≡ root.deckId
         then Set.empty
         else Set.singleton (Right deckId)

    goDeck (TraverseDeck { cards, deckId }) =
      foldMap goCard cards <> Set.singleton (Right deckId)

    isCache (Left (Card.Cache fp)) = Just fp
    isCache _ = Nothing

    isSource (Left (Card.Source fp)) = Just fp
    isSource _ = Nothing
