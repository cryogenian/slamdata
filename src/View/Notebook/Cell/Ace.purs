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

module View.Notebook.Cell.Ace (aceEditor) where

import Prelude
import Optic.Getter ((^.))

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A

import View.Common (fadeWhen, row)
import qualified View.Css as VC
import View.Notebook.Common (HTML(), dataCellId, dataCellType)
import Model.Notebook.Cell (Cell(), CellContent(), _hiddenEditor, _cellId, _content, cellContentType)


aceEditor :: forall e. Cell -> HTML e
aceEditor state =
  row [ H.div [ A.classes $ [VC.cellInput] <> fadeWhen (state ^. _hiddenEditor) ]
        [ H.div [ dataCellId $ state ^. _cellId
                , dataCellType $ state ^. _content
                , A.classes [ VC.aceContainer ]
                ]
          []
        ]
      ]
