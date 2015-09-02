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

module View.File.Breadcrumb (breadcrumbs) where

import Prelude
import Controller.File.Common (browseURL)
import Data.Maybe (Maybe(..))
import Model.File
import Model.File.Breadcrumb (Breadcrumb())
import View.File.Common (HTML())
import Optic.Core

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.Themes.Bootstrap3 as B

breadcrumbs :: forall e. State -> HTML e
breadcrumbs state =
  H.ol [ A.classes [B.breadcrumb, B.colXs7] ]
       $ breadcrumb state <$> (state ^. _breadcrumbs)

breadcrumb :: forall e. State -> Breadcrumb -> HTML e
breadcrumb state b =
  H.li_ [ H.a [ A.href (browseURL Nothing
                                  (state ^. _sort)
                                  (state ^. _salt)
                                  b.link) ]
              [ H.text b.name ]
        ]
