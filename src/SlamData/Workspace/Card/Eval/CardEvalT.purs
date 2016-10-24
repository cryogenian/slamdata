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
  , runCardEvalT'
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
import Control.Parallel.Class as Par

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

newtype CardEvalT m a = CardEvalT (CardEvalTP m a)

getCardEvalT ∷ ∀ m a. CardEvalT m a → CardEvalTP m a
getCardEvalT (CardEvalT m) = m

instance functorCardEvalT ∷ Functor m ⇒ Functor (CardEvalT m) where
  map f = getCardEvalT ⋙ map f ⋙ CardEvalT

instance applyCardEvalT ∷ Apply m ⇒ Apply (CardEvalT m) where
  apply (CardEvalT f) = getCardEvalT ⋙ apply f ⋙ CardEvalT

instance applicativeCardEvalT ∷ Applicative m ⇒ Applicative (CardEvalT m) where
  pure = pure ⋙ CardEvalT

instance bindCardEvalT ∷ Monad m ⇒ Bind (CardEvalT m) where
  bind (CardEvalT m) = (_ ⋙ getCardEvalT) ⋙ bind m ⋙ CardEvalT

instance monadCardEvalT ∷ Monad m ⇒ Monad (CardEvalT m)

instance monadTransCardEvalT ∷ MonadTrans CardEvalT where
  lift = lift ⋙ lift ⋙ CardEvalT

instance monadWriterCardEvalT
         ∷ (Monad m) ⇒ WC.MonadWriter (Set.Set AdditionalSource) (CardEvalT m) where
  writer = WC.writer ⋙ lift ⋙ CardEvalT
  listen = getCardEvalT ⋙ WC.listen ⋙ CardEvalT
  pass = getCardEvalT ⋙ WC.pass ⋙ CardEvalT

instance monadErrorCardEvalT ∷ Monad m ⇒ MonadError QError (CardEvalT m) where
  throwError = throwError ⋙ CardEvalT
  catchError (CardEvalT m) = CardEvalT ∘ catchError m ∘ (getCardEvalT ∘ _)

instance monadParCardEvalT ∷ Par.MonadPar m ⇒ Par.MonadPar (CardEvalT m) where
  par f (CardEvalT ma) (CardEvalT mb) = CardEvalT (Par.par f ma mb)

runCardEvalT
  ∷ ∀ m
  . Functor m
  ⇒ CardEvalT m Port.Port
  → m (Either GE.GlobalError (Port.Port × (Set.Set AdditionalSource)))
runCardEvalT =
  runCardEvalT' >>> map \(r × ms) →
    case r of
      Left err →
        case GE.fromQError err of
          Left msg → Right $ Port.CardError msg × ms
          Right ge → Left ge
      Right r' →
        Right $ r' × ms

runCardEvalT'
  ∷ ∀ m
  . Functor m
  ⇒ CardEvalT m Port.Port
  → m (Either QError Port.Port × Set.Set AdditionalSource)
runCardEvalT' (CardEvalT m) =
  WT.runWriterT (runExceptT m)

runCardEvalT_
  ∷ ∀ m
  . Functor m
  ⇒ CardEvalT m Unit → m Unit
runCardEvalT_ (CardEvalT m) =
  WT.runWriterT (runExceptT m) <#> \(x × _) → either (const unit) id x

addSource
  ∷ ∀ m
  . (WC.MonadWriter (Set.Set AdditionalSource) m)
  ⇒ FilePath
  → m Unit
addSource fp = WT.tell $ Set.singleton $ Source fp

addCache
  ∷ ∀ m
  . (WC.MonadWriter (Set.Set AdditionalSource) m)
  ⇒ FilePath
  → m Unit
addCache fp = WT.tell $ Set.singleton $ Cache fp

addSources
  ∷ ∀ m f
  . (Foldable f, WC.MonadWriter (Set.Set AdditionalSource) m)
  ⇒ f FilePath
  → m Unit
addSources fps = WT.tell $ foldMap (Set.singleton ∘ Source) fps

addCaches
  ∷ ∀ m f
  . (Foldable f, WC.MonadWriter (Set.Set AdditionalSource) m)
  ⇒ f FilePath
  → m Unit
addCaches fps = WT.tell $ foldMap (Set.singleton ∘ Cache) fps

additionalSources
  ∷ ∀ m f
  . (Foldable f, WC.MonadWriter (Set.Set AdditionalSource) m)
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
