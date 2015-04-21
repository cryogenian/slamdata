module View.File.Modal.ShareDialog where

import Control.Alternative (Alternative)
import Model.File
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.Themes.Bootstrap3 as B
import Utils.Halide (readonly)
import View.File.Modal.Common

shareDialog :: forall p m. (Alternative m) => (Request -> m Input) -> String -> [H.HTML p (m Input)]
shareDialog handler url =
  [ header $ h4 "Share"
  , body [ H.form_ [ H.div [ A.classes [B.formGroup]
                           , E.onClick (\ev -> pure $ handler $ ToSelect ev.target)
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
                      , E.onClick (\_ -> pure $ handler $ ToClipboard url)
                      ]
                      [ H.text "Copy" ]
           ]
  ]
