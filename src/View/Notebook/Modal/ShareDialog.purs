module View.Notebook.Modal.ShareDialog where

import Data.Maybe (Maybe(..))
import Input.Notebook
import Model.Notebook
import Optic.Core ((.~))
import Utils.Halide (dataZeroClipboard, selectThis)
import View.Modal.Common
import View.Notebook.Common (HTML())

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.Themes.Bootstrap3 as B

shareDialog :: forall e. String -> [HTML e]
shareDialog url =
  [ header $ h4 "URL"
  , body [ H.form [ nonSubmit ]
                  [ H.div [ A.classes [B.formGroup]
                          , E.onClick selectThis
                          ]
                          [ H.input [ A.classes [B.formControl]
                                    , A.value url
                                    , A.readonly true
                                    ]
                                    []
                          ]
                  ]
         ]
  , footer [ H.button [ A.id_ "copy-button"
                      , A.classes [B.btn, B.btnPrimary]
                      , E.onClick (E.input_ $ WithState (_dialog .~ Nothing))
                      , dataZeroClipboard url
                      ]
                      [ H.text "Copy" ]
           ]
  ]
