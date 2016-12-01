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

module SlamData.Workspace.Card.Eval.CardEvalT
  ( CardEvalInput
  , CardEvalT
  , addSource
  , addCache
  , addSources
  , addCaches
  , additionalSources
  , runCardEvalT
  , runCardEvalT_
  , temporaryOutputResource
  , liftQ
  ) where

import SlamData.Prelude

import Data.Map as Map
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as Path
import Data.Set as Set

import Control.Monad.Writer.Class as WC
import Control.Monad.Writer.Trans as WT
import Control.Parallel as Par

import Quasar.Error (QError)

import SlamData.GlobalError as GE
import SlamData.Workspace.Card.CardId as CID
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Deck.DeckId as DID
import SlamData.Workspace.Deck.AdditionalSource (AdditionalSource(..))


import Utils.Path (DirPath, FilePath)

type CardEvalInput =
  { path ∷ DirPath
  , input ∷ Maybe Port.Port
  , cardCoord ∷ DID.DeckId × CID.CardId
  , urlVarMaps ∷ Map.Map DID.DeckId Port.URLVarMap
  }

type CardEvalTP m = ExceptT QError (WT.WriterT (Set.Set AdditionalSource) m)

newtype CardEvalT m a = CardEvalT (ExceptT QError (WT.WriterT (Set.Set AdditionalSource) m) a)

getCardEvalT ∷ ∀ m a. CardEvalT m a → CardEvalTP m a
getCardEvalT (CardEvalT m) = m

derive newtype instance functorCardEvalT ∷ Functor m ⇒ Functor (CardEvalT m)

derive newtype instance applyCardEvalT ∷ Monad m ⇒ Apply (CardEvalT m)

derive newtype instance applicativeCardEvalT ∷ Monad m ⇒ Applicative (CardEvalT m)

derive newtype instance bindCardEvalT ∷ Monad m ⇒ Bind (CardEvalT m)

derive newtype instance monadCardEvalT ∷ Monad m ⇒ Monad (CardEvalT m)

instance monadTransCardEvalT ∷ MonadTrans CardEvalT where
  lift = lift ⋙ lift ⋙ CardEvalT

derive newtype instance monadTellCardEvalT ∷ Monad m ⇒ WC.MonadTell (Set.Set AdditionalSource) (CardEvalT m)

derive newtype instance monadWriterCardEvalT ∷ Monad m ⇒ WC.MonadWriter (Set.Set AdditionalSource) (CardEvalT m)

derive newtype instance monadErrorCardEvalT ∷ Monad m ⇒ MonadError QError (CardEvalT m)

derive newtype instance parallelCardEvalT ∷ Par.Parallel f (ExceptT QError (WT.WriterT (Set.Set AdditionalSource) m)) ⇒ Par.Parallel f (CardEvalT m)

runCardEvalT
  ∷ ∀ m
  . Functor m
  ⇒ CardEvalT m Port.Port
  → m (Either GE.GlobalError (Port.Port × (Set.Set AdditionalSource)))
runCardEvalT (CardEvalT m) =
  WT.runWriterT (runExceptT m) <#> \(r × ms) →
    case r of
      Left err →
        case GE.fromQError err of
          Left msg → Right $ Port.CardError msg × ms
          Right ge → Left ge
      Right r' →
        Right $ r' × ms

runCardEvalT_
  ∷ ∀ m
  . Functor m
  ⇒ CardEvalT m Unit → m Unit
runCardEvalT_ (CardEvalT m) =
  WT.runWriterT (runExceptT m) <#> \(x × _) → either (const unit) id x

addSource
  ∷ ∀ m
  . (WC.MonadTell (Set.Set AdditionalSource) m)
  ⇒ FilePath
  → m Unit
addSource fp = WT.tell $ Set.singleton $ Source fp

addCache
  ∷ ∀ m
  . (WC.MonadTell (Set.Set AdditionalSource) m)
  ⇒ FilePath
  → m Unit
addCache fp = WT.tell $ Set.singleton $ Cache fp

addSources
  ∷ ∀ m f
  . (Foldable f, WC.MonadTell (Set.Set AdditionalSource) m)
  ⇒ f FilePath
  → m Unit
addSources fps = WT.tell $ foldMap (Set.singleton ∘ Source) fps

addCaches
  ∷ ∀ m f
  . (Foldable f, WC.MonadTell (Set.Set AdditionalSource) m)
  ⇒ f FilePath
  → m Unit
addCaches fps = WT.tell $ foldMap (Set.singleton ∘ Cache) fps

additionalSources
  ∷ ∀ m f
  . (Foldable f, WC.MonadTell (Set.Set AdditionalSource) m)
  ⇒ f AdditionalSource
  → m Unit
additionalSources = WT.tell ∘ foldMap Set.singleton

temporaryOutputResource ∷
  ∀ r
  . { path ∷ DirPath, cardCoord ∷ DID.DeckId × CID.CardId | r }
  → FilePath
temporaryOutputResource { path, cardCoord: deckId × cardId } =
  path
    </> Path.dir ".tmp"
    </> Path.dir (DID.deckIdToString deckId)
    </> Path.file ("out" ⊕ CID.cardIdToString cardId)

liftQ ∷ ∀ m a. Monad m ⇒ m (Either QError a) → CardEvalT m a
liftQ = either throwError pure <=< lift
