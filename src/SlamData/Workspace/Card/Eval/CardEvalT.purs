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
  , CardEvalResult
  , CardEvalT
  , runCardEvalT
  , temporaryOutputResource
  ) where

import SlamData.Prelude

import Data.Path.Pathy ((</>))
import Data.Path.Pathy as Path

import Control.Monad.Error.Class as EC
import Control.Monad.Except.Trans as ET
import Control.Monad.Writer.Class as WC
import Control.Monad.Writer.Trans as WT

import SlamData.Workspace.AccessType as AT
import SlamData.Workspace.Card.CardId as CID
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Port.VarMap as VM

import Utils.Path (DirPath, FilePath)

type CardEvalInput =
  { path ∷ Maybe DirPath
  , input ∷ Maybe Port.Port
  , cardId ∷ CID.CardId
  , globalVarMap ∷ VM.VarMap
  , accessType ∷ AT.AccessType
  }

-- | The result value produced when evaluating a card.
-- |
-- | - `output` is the value that this card component produces that is taken as
-- |   the input for dependant cards.
-- | - `messages` is for any status messages that arise during
-- |   evaluation.
type CardEvalResult =
  { output ∷ Port.Port
  , messages ∷ Array String
  }

type CardEvalTP m = ET.ExceptT String (WT.WriterT (Array String) m)

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

instance monadWriterCardEvalT ∷ Monad m ⇒ WC.MonadWriter (Array String) (CardEvalT m) where
  writer = WC.writer ⋙ lift ⋙ CardEvalT
  listen = getCardEvalT ⋙ WC.listen ⋙ CardEvalT
  pass = getCardEvalT ⋙ WC.pass ⋙ CardEvalT

instance monadErrorCardEvalT ∷ Monad m ⇒ EC.MonadError String (CardEvalT m) where
  throwError = EC.throwError ⋙ CardEvalT
  catchError (CardEvalT m) = CardEvalT ∘ EC.catchError m ∘ (getCardEvalT ∘ _)

runCardEvalT ∷ ∀ m. Functor m ⇒ CardEvalT m Port.Port → m CardEvalResult
runCardEvalT (CardEvalT m) =
  WT.runWriterT (ET.runExceptT m) <#> uncurry \r ms →
    { output: either Port.CardError id r
    , messages: ms
    }

temporaryOutputResource ∷
  ∀ r
  . { path ∷ Maybe DirPath, cardId ∷ CID.CardId | r }
  → FilePath
temporaryOutputResource { path, cardId } = outputDirectory </> outputFile
  where
  outputDirectory =
    fromMaybe (Path.rootDir </> Path.dir ".tmp") $
      filterMaybe (_ == Path.rootDir) path

  outputFile = Path.file $ "out" ⊕ CID.cardIdToString cardId

  filterMaybe ∷ ∀ a. (a → Boolean) → Maybe a → Maybe a
  filterMaybe p m = m >>= \x → if p x then Nothing else pure x
