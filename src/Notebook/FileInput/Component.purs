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

module Notebook.FileInput.Component
  ( State(..)
  , Query(..)
  , fileInputComponent
  , initialState
  ) where

import Prelude

import Control.Coroutine as CR
import Control.Monad (when)
import Control.Monad.Aff (Aff())
import Control.Monad.Eff.Exception (EXCEPTION())
import Control.Monad.Free.Trans as FT

import Data.Functor
import Data.Array as A
import Data.Maybe as M
import Data.Either as E
import Data.NaturalTransformation
import Data.Path.Pathy as P

import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Events.Handler as HEH
import Halogen.Themes.Bootstrap3 as B

import Network.HTTP.Affjax (AJAX())
import Render.CssClasses as CSS
import Quasar.Aff as API

import Model.Resource as R

type State =
  { files :: Array R.Resource
  , selectedFile :: M.Maybe R.Resource
  , currentFilePath :: String
  , showFiles :: Boolean
  }

initialState :: State
initialState =
  { files: []
  , selectedFile: M.Nothing
  , currentFilePath: ""
  , showFiles: false
  }

data Query a
  = ToggleFileList a
  | SelectFile R.Resource a
  | GetSelectedFile (M.Maybe R.Resource -> a)
  | UpdateFile String a

type Effects e =
  API.RetryEffects
    ( ajax :: AJAX
    , err :: EXCEPTION
    | e
    )

fileInputComponent :: forall e. Component State Query (Aff (Effects e))
fileInputComponent = component render eval

appendFiles :: Array R.Resource -> State -> State
appendFiles files state =
  state
    { files = A.sort $ A.nub $ state.files <> files
    }

eval :: forall e. Natural Query (ComponentDSL State Query (Aff (Effects e)))
eval q =
  case q of
    ToggleFileList next -> do
      shouldShowFiles <- get <#> _.showFiles >>> not
      modify (_ { showFiles = shouldShowFiles })
      when shouldShowFiles $ do
        let
          fileProducer =
            FT.hoistFreeT liftH $
              API.transitiveChildrenProducer P.rootDir
          fileConsumer =
            CR.consumer \fs -> do
              modify $ appendFiles fs
              pure M.Nothing
        CR.runProcess (fileProducer CR.$$ fileConsumer)
      pure next
    SelectFile r next -> do
      modify \state ->
        state
          { selectedFile = M.Just r
          , currentFilePath = R.resourcePath r
          , showFiles = false
          }
      pure next
    GetSelectedFile k ->
      k <$> gets _.selectedFile
    UpdateFile path next -> do
      modify (_ { currentFilePath = path })
      case R.fileResourceFromString path of
        E.Left str -> pure unit
        E.Right res -> modify (_ { selectedFile = M.Just res })
      pure next

render :: State -> ComponentHTML Query
render st =
  H.div_
    [ H.div
        [ HP.classes [ B.inputGroup , CSS.fileListField ] ]
        [ H.input $
            [ HP.class_ B.formControl
            , HP.placeholder "Select a file"
            , HE.onValueInput (HE.input UpdateFile)
            , HP.value st.currentFilePath
            ]
        , H.span
            [ HP.class_ B.inputGroupBtn ]
            [ H.button
                [ HP.classes [ B.btn , B.btnDefault ]
                , HP.buttonType HP.ButtonButton
                , HE.onClick \_ -> HEH.stopPropagation $> action ToggleFileList
                ]
                [ H.span [ HP.class_ B.caret ] [ ]
                ]
            ]
        ]
    , H.ul
        [ HP.classes $
            [ CSS.fileListGroup
            , B.listGroup
            , B.fade
            ] <> if st.showFiles then [ B.in_ ] else [ ]
        ]
        $ renderItem <$> st.files
    ]

renderItem
  :: R.Resource
  -> ComponentHTML Query
renderItem r =
  H.button
    [ HP.classes $
        [ B.listGroupItem
        ] <> if R.isHidden r then [ CSS.itemHidden ] else [ ]
    , HE.onClick \_ -> pure $ action (SelectFile r)
    ]
    [ H.text $ R.resourcePath r
    ]
