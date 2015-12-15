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

module Dialog.Render where

import Prelude

import Data.Functor (($>))
import Data.Maybe (Maybe(..))

import Halogen.CustomProps as Cp
import Halogen.HTML as H
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import Halogen.Themes.Bootstrap3 as B

import Render.Common (classedDiv)

modalDialog :: forall f p. Array (H.HTML p (f Unit)) -> H.HTML p (f Unit)
modalDialog children =
  (classedDiv B.modalDialog)
  [ H.div [ Cp.mbClick (\_ -> E.stopPropagation $> Nothing)
          , P.classes [ B.modalContent ]
          ]
    children
  ]

modalHeader :: forall f p. String -> H.HTML p (f Unit)
modalHeader message =
  classedDiv B.modalHeader
  [ H.h4_ [ H.text message ] ]

modalBody :: forall f p. H.HTML p (f Unit) -> H.HTML p (f Unit)
modalBody = classedDiv B.modalBody <<< pure

modalFooter :: forall f p. Array (H.HTML p (f Unit)) -> H.HTML p (f Unit)
modalFooter = classedDiv B.modalFooter
