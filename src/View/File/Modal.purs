module View.File.Modal (modal) where

import Control.Functor (($>))
import Control.Inject1 (inj)
import Control.Plus (empty)
import Data.Maybe (Maybe(..), maybe)
import EffectTypes (FileAppEff())
import Input.File (Input(), FileInput(SetDialog))
import Model.File.Dialog (Dialog(..))
import Model.File (State())
import View.File.Modal.Common (header, h4, body, footer)
import View.File.Modal.MountDialog (mountDialog)
import View.File.Modal.RenameDialog (renameDialog)
import View.File.Modal.ShareDialog (shareDialog)
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.Themes.Bootstrap3 as B

modal :: forall p e. State -> H.HTML p (E.Event (FileAppEff e) Input)
modal state =
  H.div [ A.classes ([B.modal, B.fade] <> maybe [] (const [B.in_]) state.dialog)
        , E.onClick (E.input_ $ inj $ SetDialog Nothing)
        ]
        [ H.div [ A.classes [B.modalDialog] ]
                [ H.div [ E.onClick (\_ -> E.stopPropagation $> empty)
                        , A.classes [B.modalContent]
                        ]
                        (modalContent state.dialog)
                ]
        ]

modalContent :: forall p e. Maybe Dialog -> [H.HTML p (E.Event (FileAppEff e) Input)]
modalContent Nothing = []
modalContent (Just dialog) = dialogContent dialog

dialogContent :: forall p e. Dialog -> [H.HTML p (E.Event (FileAppEff e) Input)]
dialogContent (ShareDialog url) = shareDialog url
dialogContent (RenameDialog dialog) = renameDialog dialog
dialogContent (MountDialog dialog) = mountDialog dialog

emptyDialog :: forall p i. String -> [H.HTML p i]
emptyDialog title =
    [ header $ h4 title
    , body []
    , footer []
    ]
