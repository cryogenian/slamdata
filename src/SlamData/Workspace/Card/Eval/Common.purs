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

module SlamData.Workspace.Card.Eval.Common where

import SlamData.Prelude

import Control.Monad.Aff.Class (class MonadAff)
import Data.Lens ((^?), _Right, (<>~), (%~))
import Data.List as L
import Data.Path.Pathy as Path
import Data.StrMap as SM
import Matryoshka (embed)
import Quasar.Advanced.QuasarAF as QF
import Quasar.Types (FilePath)
import SlamData.Effects (SlamDataEffects)
import SlamData.Quasar.Class (class QuasarDSL, class ParQuasarDSL, sequenceQuasar)
import SlamData.Quasar.Error as QE
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port as Port
import SqlSquared as Sql

validateResources
  ∷ ∀ m v t
  . MonadAff SlamDataEffects m
  ⇒ MonadThrow (Variant (qerror ∷ CE.QError | v)) m
  ⇒ QuasarDSL m
  ⇒ ParQuasarDSL m
  ⇒ Traversable t
  ⇒ t FilePath
  → m Unit
validateResources fs = do
  res ← sequenceQuasar (map (\path → Tuple path <$> QF.fileMetadata path) fs)
  for_ res case _ of
    path × Left reason →
      CE.throwQError $ QE.prefixMessage ("Resource `" ⊕ Path.printPath path ⊕ "` is unavailable") reason
    _ →
      pure unit

evalComposite
  ∷ ∀ m
  . MonadAsk CEM.CardEnv m
  ⇒ m Port.DataMap
evalComposite = do
  CEM.CardEnv { children } ← ask
  pure $ foldl mergeChildren SM.empty children
  where
  mergeChildren ∷ Port.DataMap → CEM.ChildOut → Port.DataMap
  mergeChildren vm { namespace, varMap } = case namespace of
    "" → SM.fold (update id) vm varMap
    ns → SM.fold (update \k → ns <> "." <> k) vm varMap

  alterFn
    ∷ Port.Resource ⊹ Port.VarMapValue
    → Maybe (Port.Resource ⊹ Port.VarMapValue)
    → Maybe (Port.Resource ⊹ Port.VarMapValue)
  alterFn val = case _ of
    Nothing → Just val
    Just v → case v ^? _Right ∘ Port._VarMapValue ∘ Sql._SetLiteral of
      Nothing →
        let
          v1 ∷ Sql.Sql
          v1 = toValue val
          v2 ∷ Sql.Sql
          v2 = toValue v

        in
          Just
            if v1 ≡ v2
              then val
              else Right $ Port.VarMapValue $ embed $ Sql.SetLiteral $ L.fromFoldable [ v1, v2 ]
      Just _ → Just case val ^? _Right ∘ Port._VarMapValue ∘ Sql._SetLiteral of
        Nothing →
          v # _Right ∘ Port._VarMapValue ∘ Sql._SetLiteral
            %~ (\s → let val' = toValue val
                     in if L.elem val' s
                        then s
                        else L.snoc s val')
        Just news →
          v # _Right ∘ Port._VarMapValue ∘ Sql._SetLiteral <>~ news

  update
    ∷ (String → String)
    → Port.DataMap
    → String
    → Port.Resource ⊹ Port.VarMapValue
    → Port.DataMap
  update mkKey acc key val =
    SM.alter (alterFn val) (mkKey key) acc

  toValue ∷ Port.Resource ⊹ Port.VarMapValue → Sql.Sql
  toValue =
    unwrap ∘ either Port.resourceToVarMapValue id
