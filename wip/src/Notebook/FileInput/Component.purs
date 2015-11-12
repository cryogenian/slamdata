module Notebook.FileInput.Component
  ( State(..)
  , Query(..)
  , fileInputComponent
  , initialState
  ) where

import Prelude
import Data.Functor
import Data.Array as A
import Data.Maybe as M
import Data.Either as E
import Data.NaturalTransformation

import Halogen
import Halogen.HTML.CSS.Indexed as HP
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Events.Handler as HEH

import Halogen.Themes.Bootstrap3 as B
import Render.CssClasses as CSS

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
  | UpdateFile String a

fileInputComponent :: forall g. Component State Query g
fileInputComponent = component render eval

eval :: forall g. Natural Query (ComponentDSL State Query g)
eval q =
  case q of
    ToggleFileList next -> do
      modify (_ { showFiles = false })
      pure next
    SelectFile r next -> do
      modify \state ->
        state
          { selectedFile = M.Just r
          , currentFilePath = R.resourcePath r
          , showFiles = false
          }
      pure next
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
