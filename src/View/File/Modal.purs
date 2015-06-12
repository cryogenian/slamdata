module View.File.Modal (modal) where

import Control.Functor (($>))
import Control.Plus (empty)
import Data.Inject1 (inj)
import Data.Maybe (Maybe(..), maybe)
import Input.File (FileInput(..))
import Model.File (State(), _dialog)
import Model.File.Dialog (Dialog(..))
import Optic.Core ((.~), (^.))
import View.File.Common (HTML())
import View.File.Modal.ErrorDialog (errorDialog)
import View.File.Modal.MountDialog (mountDialog)
import View.File.Modal.RenameDialog (renameDialog)
import View.File.Modal.ShareDialog (shareDialog)
import View.Modal.Common (header, h4, body, footer)

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.Themes.Bootstrap3 as B

modal :: forall e. State -> HTML e
modal state =
  H.div [ A.classes ([B.modal, B.fade] <> maybe [] (const [B.in_]) (state ^. _dialog))
        , E.onClick (E.input_ $ inj $ WithState (_dialog .~ Nothing))
        ]
        [ H.div [ A.classes [B.modalDialog] ]
                [ H.div [ E.onClick (\_ -> E.stopPropagation $> empty)
                        , A.classes [B.modalContent]
                        ]
                        $ maybe [] dialogContent (state ^. _dialog)
                ]
        ]

dialogContent :: forall e. Dialog -> [HTML e]
dialogContent (ShareDialog url) = shareDialog url
dialogContent (RenameDialog dialog) = renameDialog dialog
dialogContent (MountDialog dialog) = mountDialog dialog
dialogContent (ErrorDialog msg) = errorDialog msg
