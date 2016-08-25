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

module SlamData.Workspace.Card.Draftboard.Common
  ( childDeckIds
  , deleteGraph
  , replacePointer
  , unsafeUpdateCachedDraftboard
  ) where

import SlamData.Prelude

import Control.Monad.Aff.Free (class Affable)

import Data.Array as Array
import Data.Map as Map
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as Pathy

import SlamData.Effects (SlamDataEffects)
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Data as Quasar
import SlamData.Quasar.Error as QE
import SlamData.Workspace.Card.CardId (CardId)
import SlamData.Workspace.Card.Model as CM
import SlamData.Workspace.Deck.DeckId (DeckId, deckIdToString)
import SlamData.Workspace.Deck.Model as DM
import SlamData.Wiring (Wiring(..), putCardEval, getCache)

import Utils.Path (DirPath)

transitiveChildren
  ∷ ∀ m
  . (MonadPar m, QuasarDSL m)
  ⇒ DirPath
  → DeckId
  → ExceptT QE.QError m (Array DeckId)
transitiveChildren path = go
  where
  go ∷ DeckId → ExceptT QE.QError m (Array DeckId)
  go parentId = do
    cids ← loadChildIds parentId
    ccids ← parTraverse go cids
    pure $ cids <> join ccids

  loadChildIds ∷ DeckId → ExceptT QE.QError m (Array DeckId)
  loadChildIds parentId = do
    json ← ExceptT $ Quasar.load (DM.deckIndex path parentId)
    ExceptT $ pure $
      childDeckIds ∘ _.cards <$> lmap QE.msgToQError (DM.decode json)

childDeckIds ∷ Array (CM.Model) → Array DeckId
childDeckIds = (_ >>= getDeckIds ∘ _.model)
  where
  getDeckIds =
    case _ of
      CM.Draftboard { decks } → Array.fromFoldable $ Map.keys decks
      _ → []

deleteGraph
  ∷ ∀ m
  . (MonadPar m, QuasarDSL m)
  ⇒ DirPath
  → DeckId
  → m (Either QE.QError Unit)
deleteGraph path parentId = runExceptT do
  cids ← transitiveChildren path parentId
  void
    $ ExceptT
    $ map sequence
    $ parTraverse delete
    $ Array.cons parentId cids

  where
  delete deckId =
    Quasar.delete $ Left $ path </> Pathy.dir (deckIdToString deckId)

replacePointer
  ∷ DeckId
  → Maybe DeckId
  → CardId
  → Array (CM.Model)
  → Array (CM.Model)
replacePointer from to cid = map replace
  where
  replace model =
    case model of
      { cardId, model: CM.Draftboard { decks } } | cardId ≡ cid →
        { cardId, model: CM.Draftboard { decks: update decks } }
      _ → model

  update decks =
    case Map.lookup from decks of
      Nothing → decks
      Just rect →
        decks
          # Map.delete from
          # maybe id (flip Map.insert rect) to

-- | This shouldn't be done in general, but since draftboards have no inputs or
-- | outputs it's OK to just swap out the model for the cached card eval.
unsafeUpdateCachedDraftboard
  ∷ ∀ m
  . (Monad m, Affable SlamDataEffects m, MonadReader Wiring m)
  ⇒ DeckId
  → CM.Model
  → m Unit
unsafeUpdateCachedDraftboard deckId model = do
  Wiring wiring ← ask
  case model of
    { cardId, model: CM.Draftboard db } → do
      let coord = deckId × cardId
      getCache coord wiring.cards >>= traverse_ \ce → do
        let card = map (_ { model = CM.Draftboard db }) ce.card
        putCardEval (ce { card = card }) wiring.cards
    _ → pure unit
