{-
Copyright 2015 SlamData, Inc.

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

module App.File (app) where

import Prelude
import EffectTypes (FileAppEff())
import Halogen.Component (Component())
import Halogen.HTML.Events.Monad (Event())
import Halogen.Signal (stateful)
import Input.File (Input(), updateState)
import Model.File (initialState)
import View.File (fileView)

app :: forall e. Component (Event (FileAppEff e)) Input Input
app = fileView <$> stateful initialState updateState
