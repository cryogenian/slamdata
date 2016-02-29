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

module SlamData.Notebook.FileInput.Component
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
import Data.Bifunctor (lmap)
import Data.Array as A
import Data.Maybe as M
import Data.Either as E
import Data.NaturalTransformation
import Data.Path.Pathy as P

import DOM (DOM())

import Halogen
import Halogen.HTML.Events.Handler as HEH
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B

import Network.HTTP.Affjax (AJAX())

import Quasar.Aff as API
import Quasar.Auth as Auth
import Quasar.Auth.Permission as Auth

import SlamData.FileSystem.Resource as R
import SlamData.Render.CSS as CSS

type State =
  { files :: Array R.Resource
  , selectedFile :: E.Either String R.Resource
  , currentFilePath :: String
  , showFiles :: Boolean
  }

initialState :: State
initialState =
  { files: []
  , selectedFile: E.Left "No file selected"
  , currentFilePath: ""
  , showFiles: false
  }

data Query a
  = ToggleFileList a
  | SelectFile R.Resource a
  | GetSelectedFile (E.Either String R.Resource -> a)
  | UpdateFile String a

type Effects e =
  API.RetryEffects
    ( ajax :: AJAX
    , err :: EXCEPTION
    , dom :: DOM
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
        idToken <- liftEff' Auth.retrieveIdToken
        perms <- liftEff' Auth.retrievePermissionTokens
        let
          fileProducer =
            FT.hoistFreeT liftH $
              API.transitiveChildrenProducer P.rootDir idToken perms
          fileConsumer =
            CR.consumer \fs -> do
              modify $ appendFiles fs
              pure M.Nothing
        CR.runProcess (fileProducer CR.$$ fileConsumer)
      pure next
    SelectFile r next -> do
      modify \state ->
        state
          { selectedFile = pure r
          , currentFilePath = R.resourcePath r
          , showFiles = false
          }
      pure next
    GetSelectedFile k ->
      k <$> gets _.selectedFile
    UpdateFile "" next -> do
      modify $ _{ selectedFile = E.Left "No file selected"
                , currentFilePath = ""
                }
      pure next
    UpdateFile path next -> do
      modify $ _{ selectedFile =
                     lmap (\x -> "\"" <> x <> "\" is incorrect path for file")
                     $ R.fileResourceFromString path
                , currentFilePath = path
                }
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
