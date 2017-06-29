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

module SlamData.Dialog.Render where

import SlamData.Prelude

import Halogen.HTML as H

import SlamData.Render.Common (classedDiv)

modalDialog :: forall f p. Array (H.HTML p (f Unit)) -> H.HTML p (f Unit)
modalDialog children =
  -- I think we don't need stopPropagation and preventDefault anymore
  (classedDiv $ H.ClassName "deck-dialog")
  [ H.div_
    children
  ]

modalHeader :: forall f p. String -> H.HTML p (f Unit)
modalHeader message =
  H.h4_ [ H.text message ]

modalBody :: forall f p. H.HTML p (f Unit) -> H.HTML p (f Unit)
modalBody = classedDiv (H.ClassName "deck-dialog-body") <<< pure

modalFooter :: forall f p. Array (H.HTML p (f Unit)) -> H.HTML p (f Unit)
modalFooter = classedDiv (H.ClassName "deck-dialog-footer")

