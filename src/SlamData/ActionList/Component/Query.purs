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

module SlamData.ActionList.Component.Query where

import SlamData.Prelude

import DOM.HTML.Types (HTMLElement)
import SlamData.ActionList.Action (Action)
import SlamData.ActionList.Component.ActionInternal (ActionInternal, Dimensions)

data Query a b
  = Selected (ActionInternal a) b
  | UpdateFilter String b
  | UpdateActions (Array (Action a)) b
  | CalculateBoundingRect b
  | GetBoundingRect (Maybe Dimensions → b)
  | SetBoundingRect Dimensions b
  | SetBoundingElement (Maybe HTMLElement) b
