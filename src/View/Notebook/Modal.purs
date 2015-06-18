module View.Notebook.Modal (modal) where

import Control.Functor (($>))
import Control.Plus (empty)
import Data.Maybe (Maybe(..), maybe)
import Input.Notebook
import Model.Notebook
import Model.Notebook.Dialog
import Optic.Core ((.~))
import View.Modal.Common (header, h4, body, footer)
import View.Notebook.Common (HTML())
import View.Notebook.Modal.ErrorDialog (errorDialog)
import View.Notebook.Modal.EmbedDialog (embedDialog)

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.Themes.Bootstrap3 as B

modal :: forall e. State -> HTML e
modal state =
  H.div [ A.classes ([B.modal, B.fade] <> maybe [] (const [B.in_]) state.dialog)
        , E.onClick (E.input_ $ WithState $ _dialog .~ Nothing)
        ]
        [ H.div [ A.classes [B.modalDialog] ]
                [ H.div [ E.onClick (\_ -> E.stopPropagation $> empty)
                        , A.classes [B.modalContent]
                        ]
                        (maybe [] dialogContent state.dialog)
                ]
        ]

dialogContent :: forall e. Dialog -> [HTML e]
dialogContent (ErrorDialog msg) = errorDialog msg
dialogContent (EmbedDialog url) = embedDialog url
