module View.File.Modal.ErrorDialog (errorDialog) where

import Prelude
import Data.Inject1 (inj)
import Data.Maybe (Maybe(..))
import Input.File (FileInput(..))
import Model.File (_dialog)
import Optic.Setter ((.~))
import View.File.Common (HTML())
import View.Modal.Common (header, body, footer, h4)

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.Themes.Bootstrap3 as B

errorDialog :: forall e. String -> Array (HTML e)
errorDialog message =
  [ header $ h4 "Error"
  , body [ H.div [ A.classes [B.alert, B.alertDanger] ]
                 [ H.text message ]
         ]
  , footer [ H.button [ A.classes [B.btn]
                      , E.onClick (E.input_ $ inj $ WithState (_dialog .~ Nothing))
                      ]
                      [ H.text "Dismiss" ]
           ]
  ]
