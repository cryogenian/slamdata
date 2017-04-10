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

module SlamData.Workspace.Card.Open.Item where

import SlamData.Prelude
import SlamData.FileSystem.Resource as R
import SlamData.Workspace.Card.Open.Model as Open
import SlamData.Workspace.Card.Port.VarMap as VM

import Utils.Path (AnyPath)

data AnyItem a
  = Root
  | Variables
  | Variable VM.Var
  | Resource a

anyItemToOpen ∷ AnyItem' → Maybe Open.Open
anyItemToOpen = case _ of
  Variable v → Just (Open.Variable v)
  Resource r → Just (Open.Resource r)
  _ → Nothing

derive instance eqAnyItem ∷ Eq a ⇒ Eq (AnyItem a)
derive instance ordAnyItem ∷ Ord a ⇒ Ord (AnyItem a)
derive instance functorAnyItem ∷ Functor AnyItem

type AnyPath' = AnyItem AnyPath
type AnyItem' = AnyItem R.Resource
