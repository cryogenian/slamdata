module View.File.Modal.ShareDialog where

import Data.Maybe
import Model.File
import EffectTypes
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.Themes.Bootstrap3 as B
import Utils.Halide (readonly)
import Controller.File (selectThis)
import View.File.Modal.Common

shareDialog :: forall p e. String -> [H.HTML p (E.Event (FileAppEff e) Input)]
shareDialog url =
  [ header $ h4 "URL"
  , body [ H.form_ [ H.div [ A.classes [B.formGroup]
                           , E.onClick selectThis
                           ]
                           [ H.input [
                                     A.classes [B.formControl]
                                     , A.value url
                                     , readonly true
                                     ]
                                     []
                           ]
                   ]
         ]
  , footer [ H.button [ A.id_ "copy-button"
                      , A.classes [B.btn, B.btnPrimary]
                      , E.onClick (E.input_ $ SetDialog Nothing)
                      ]
                      [ H.text "Copy" ]
           ]
  ]
