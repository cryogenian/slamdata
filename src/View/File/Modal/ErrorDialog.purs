module View.File.Modal.ErrorDialog where

import Data.Inject1 (inj)
import Data.Maybe (Maybe(..))
import Input.File
import Model.File
import Optic.Core ((.~))
import View.File.Common (I())
import View.Modal.Common

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.Themes.Bootstrap3 as B

errorDialog :: forall e. String -> [H.HTML (I e)]
errorDialog message =
  [ header $ h4 "Error"
  , body [ H.div [ A.classes [B.alert, B.alertDanger] ]
                 [ H.text message ]
         ]
  , footer [ H.button [ A.classes [B.btn]
                      , E.onClick (E.input_ $ inj $ SetDialog Nothing)
                      ]
                      [ H.text "Dismiss" ]
           ]
  ]
