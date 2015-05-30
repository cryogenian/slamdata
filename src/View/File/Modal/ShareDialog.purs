module View.File.Modal.ShareDialog where

import Data.Inject1 (inj)
import Data.Maybe
import EffectTypes
import Input.File (FileInput(SetDialog))
import Model.File
import Utils.Halide (dataZeroClipboard, selectThis)
import View.File.Common (I())
import View.Modal.Common

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.Themes.Bootstrap3 as B

shareDialog :: forall e. String -> [H.HTML (I e)]
shareDialog url =
  [ header $ h4 "URL"
  , body [ H.form [ nonSubmit ]
           [ H.div [ A.classes [B.formGroup]
                   , E.onClick selectThis
                   ]
             [ H.input [
                  A.classes [B.formControl]
                  , A.value url
                  , A.readonly true
                  ]
               []
             ]
           ]
         ]
  , footer [ H.button [ A.id_ "copy-button"
                      , A.classes [B.btn, B.btnPrimary]
                      , E.onClick (E.input_ $ inj $ SetDialog Nothing)
                      , dataZeroClipboard url
                      ]
                      [ H.text "Copy" ]
           ]
  ]
