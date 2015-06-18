module View.Notebook.Modal.EmbedDialog (embedDialog) where

import Data.Inject1 (inj)
import Data.Maybe (Maybe(..))
import Input.Notebook (Input(..))
import Model.Notebook (_dialog)
import Optic.Core ((.~))
import Utils.Halide (dataZeroClipboard, selectThis)
import View.Notebook.Common (HTML())
import View.Modal.Common (header, body, footer, h4, nonSubmit)

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.Themes.Bootstrap3 as B
import qualified View.Css as VC

embedDialog :: forall e. String -> [HTML e]
embedDialog url =
  let code = """<iframe src=""" ++ "\"" ++ url ++ "\"" ++ """ width="100%" height="500" frameBorder="0"></iframe>"""
  in [ header $ h4 "Embed cell"
     , body [ H.form [ nonSubmit ]
                     [ H.div [ A.classes [B.formGroup]
                             , E.onClick selectThis
                             ]
                             [ H.textarea [ A.classes [B.formControl, VC.embedBox]
                                          , A.value code
                                          , A.readonly true
                                          ]
                                          []
                             ]
                     ]
            ]
     , footer [ H.button [ A.id_ "copy-button"
                         , A.classes [B.btn, B.btnPrimary]
                         , E.onClick (E.input_ $ inj $ WithState (_dialog .~ Nothing))
                         , dataZeroClipboard code
                         ]
                         [ H.text "Copy" ]
              ]
     ]

