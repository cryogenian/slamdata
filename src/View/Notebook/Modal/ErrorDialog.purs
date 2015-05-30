module View.Notebook.Modal.ErrorDialog where

import Data.Maybe (Maybe(..))
import Input.Notebook
import Model.Notebook
import Optic.Core ((.~))
import View.Modal.Common
import View.Notebook.Common (HTML())

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.Themes.Bootstrap3 as B

errorDialog :: forall e. String -> [HTML e]
errorDialog message =
  [ header $ h4 "Error"
  , body [ H.div [ A.classes [B.alert, B.alertDanger] ]
                 [ H.text message ]
         ]
  , footer [ H.button [ A.classes [B.btn]
                      , E.onClick (E.input_ $ WithState (_dialog .~ Nothing))
                      ]
                      [ H.text "Dismiss" ]
           ]
  ]
