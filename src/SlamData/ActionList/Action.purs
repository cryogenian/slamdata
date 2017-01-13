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

module SlamData.ActionList.Action where

import SlamData.Prelude

newtype ActionName = ActionName String

newtype ActionIconSrc = ActionIconSrc String
derive newtype instance eqActionIconSrc ∷ Eq ActionIconSrc

newtype ActionDescription = ActionDescription String
derive newtype instance eqActionDescription ∷ Eq ActionDescription

newtype ActionHighlighted = ActionHighlighted Boolean
derive newtype instance eqActionHighlighted ∷ Eq ActionHighlighted

newtype ActionDisabled = ActionDisabled Boolean
derive newtype instance eqActionDisabled ∷ Eq ActionDisabled

data Action a
  = Do ActionName ActionIconSrc ActionDescription ActionHighlighted ActionDisabled a
  | Drill ActionName ActionIconSrc ActionDescription (Array (Action a))
