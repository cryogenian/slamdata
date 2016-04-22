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

import SlamData.Prelude

import Control.Coroutine as CR
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar as AVar
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref as Ref
import Control.Monad.Free.Trans as FT

import Data.Array as A
import Data.Path.Pathy as P
import Data.Date as Date

import DOM (DOM)

import Halogen as H
import Halogen.HTML.Events.Handler as HEH
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import Network.HTTP.Affjax (AJAX)

import Quasar.Types (FilePath)

import SlamData.FileSystem.Resource as R
import SlamData.Quasar.FS as API
import SlamData.Render.CSS as CSS

import Utils.Path as PU

type State =
  { files :: Array R.Resource
  , selectedFile :: Either String FilePath
  , currentFilePath :: String
  , showFiles :: Boolean
  }

initialState :: State
initialState =
  { files: []
  , selectedFile: Left "No file selected"
  , currentFilePath: ""
  , showFiles: false
  }

data Query a
  = ToggleFileList a
  | SelectFile R.Resource a
  | GetSelectedFile (Either String FilePath -> a)
  | UpdateFile String a

type Effects e =
    ( avar :: AVar.AVAR
    , now :: Date.Now
    , ref :: Ref.REF
    , ajax :: AJAX
    , err :: EXCEPTION
    , dom :: DOM
    | e
    )

fileInputComponent :: forall e. H.Component State Query (Aff (Effects e))
fileInputComponent = H.component { render, eval }

appendFiles :: Array R.Resource -> State -> State
appendFiles files state =
  state
    { files = A.sort $ A.nub $ state.files <> files
    }

eval :: forall e. Natural Query (H.ComponentDSL State Query (Aff (Effects e)))
eval q =
  case q of
    ToggleFileList next -> do
      shouldShowFiles <- H.get <#> _.showFiles >>> not
      H.modify (_ { showFiles = shouldShowFiles })
      when shouldShowFiles $ do
        let
          fileProducer =
            FT.hoistFreeT H.liftH $ API.transitiveChildrenProducer P.rootDir
          fileConsumer =
            CR.consumer \fs -> H.modify (appendFiles fs) $> Nothing
        CR.runProcess (fileProducer CR.$$ fileConsumer)
      pure next
    SelectFile r next ->
      case R.getPath r of
        Left _ -> pure next
        Right file -> do
          H.modify \state ->
            state
              { selectedFile = pure file
              , currentFilePath = R.resourcePath r
              , showFiles = false
              }
          pure next
    GetSelectedFile k ->
      k <$> H.gets _.selectedFile
    UpdateFile "" next -> do
      H.modify $ _{ selectedFile = Left "No file selected"
                , currentFilePath = ""
                }
      pure next
    UpdateFile path next -> do
      H.modify $ _{ selectedFile =
                     lmap (\x -> "\"" <> x <> "\" is an incorrect path for a file")
                     $ fileFromString path
                , currentFilePath = path
                }
      pure next

render :: State -> H.ComponentHTML Query
render st =
  HH.div_
    $ [ HH.div
          [ HP.classes [ B.inputGroup , CSS.fileListField ] ]
          [ HH.input $
              [ HP.class_ B.formControl
              , HP.placeholder "Select a file"
              , HE.onValueInput (HE.input UpdateFile)
              , HP.value st.currentFilePath
              ]
          , HH.span
              [ HP.class_ B.inputGroupBtn ]
              [ HH.button
                  [ HP.classes [ B.btn , B.btnDefault ]
                  , HP.buttonType HP.ButtonButton
                  , ARIA.label toggleLabel
                  , HP.title toggleLabel
                  , HE.onClick \_ -> HEH.stopPropagation $> H.action ToggleFileList
                  ]
                  [ HH.span [ HP.class_ B.caret ] [ ]
                  ]
              ]
          ]
      ] <> fileList
  where
  toggleLabel = if st.showFiles then "Hide file list" else "Show file list"
  fileList =
    if st.showFiles
    then
      [ HH.ul
          [ HP.classes $
              [ CSS.fileListGroup
              , B.listGroup
              ]
          ]
          $ renderItem <$> st.files
      ]
    else []

renderItem :: R.Resource -> H.ComponentHTML Query
renderItem r =
  HH.button
    [ HP.classes $
        [ B.listGroupItem
        ] <> if R.isHidden r then [ CSS.itemHidden ] else [ ]
    , HE.onClick \_ -> pure $ H.action (SelectFile r)
    ]
    [ HH.text $ R.resourcePath r
    ]

fileFromString :: String -> Either String FilePath
fileFromString path = maybe (Left path) Right (PU.parseFilePath path)
