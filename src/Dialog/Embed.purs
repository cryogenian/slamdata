{-
Copyright 2015 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Dialog.Embed where

import Prelude

import Control.Monad.Aff (Aff())
import Control.UI.Browser (select, encodeURIComponent)
import Control.UI.ZClipboard as Z

import Data.Foldable as F
import Data.Functor.Eff (liftEff)
import Data.StrMap as SM
import Data.String.Regex as Rx

import Halogen
import Halogen.CustomProps as Cp
import Halogen.HTML as H
import Halogen.HTML.Indexed as HI
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import Halogen.HTML.Properties.Indexed as PI
import Halogen.HTML.Renderer.String (renderHTML)
import Halogen.Themes.Bootstrap3 as B

import DOM.HTML.Types (HTMLElement(), htmlElementToElement)

import Notebook.Cell.Port.VarMap as Port

import Dialog.Render (modalDialog, modalHeader, modalBody, modalFooter)
import Render.CssClasses as Rc

type Slam e = Aff (HalogenEffects (zClipboard :: Z.ZCLIPBOARD |e))
data State = State String Port.VarMap

data Query a
  = SelectElement HTMLElement a
  | InitZClipboard String HTMLElement a
  | Dismiss a

comp :: forall e. Component State Query (Slam e)
comp = component render eval

render :: State -> ComponentHTML Query
render (State url varMap) =
  modalDialog
    [ modalHeader "Embed cell"
    , modalBody $
        H.form
          [ Cp.nonSubmit ]
          [ H.div
              [ P.classes [ B.formGroup ]
              , E.onClick (\ev -> pure $ SelectElement ev.target unit)
              ]
              [ H.textarea
                  [ P.classes [ B.formControl, Rc.embedBox ]
                  , Cp.readonly
                  , P.value code
                  ]
              ]
          ]
    , modalFooter
        [ H.button
            [ P.id_ "copy-button"
            , P.classes [ B.btn, B.btnPrimary ]
            , E.onClick (E.input_ Dismiss)
            , P.initializer \el -> action $ InitZClipboard code el
            ]
            [ H.text "Copy"
            ]
        ]
    ]
  where
  code :: String
  code =
    renderHTML $
      HI.script
        [ PI.mediaType { type: "text", subtype: "javascript", parameters: [] }
        ]
        [ HI.text javascriptCode
        ]

  statements :: Array String -> String
  statements = F.foldl (\m x -> m <> x <> ";\n") ""

  wrapJS :: Array String -> String
  wrapJS xs = "\n" <> statements [ applyJS (absJS [] xs) [] ]

  absJS :: Array String -> Array String -> String
  absJS args xs =
    parens $
      "function" <> parens (commaSep args) <> "{\n"
        <> statements (indent <$> xs)
        <> "}"

  indent :: String -> String
  indent x = "    " <> x

  parens :: String -> String
  parens x = "(" <> x <> ")"

  applyJS :: String -> Array String -> String
  applyJS f xs =
    f <> parens (commaSep xs)

  commaSep :: Array String -> String
  commaSep = F.intercalate ","

  declVar :: String -> String -> String
  declVar k v = "var " <> k <> " = " <> v

  quotes :: String -> String
  quotes x = "\"" <> x <> "\""

  escapeQuotes :: String -> String
  escapeQuotes =
    Rx.replace
      (Rx.regex "\"" $ Rx.noFlags { global = true })
      "\\\""

  javascriptCode :: String
  javascriptCode =
    wrapJS $
      decls varMap
        <> [ writeIFrame ]

  writeIFrame :: String
  writeIFrame =
    applyJS "document.writeln"
      [ iframeHTML
      ]

  iframeSource :: String
  iframeSource =
    url <>
      if SM.isEmpty varMap
         then ""
         else "/?" <> F.intercalate "&" (printArg <$> SM.keys varMap)
    where
      printArg k =
        k <> "=\" +" <> k <> "+ \""

  iframeHTML :: String
  iframeHTML =
    quotes $
      "<iframe src=\\\""
        <> iframeSource
        <> "\\\" width=\\\"100%\\\" height=\\\"100%\\\" frameborder=\\\"0\\\">"
        <> "</iframe>"


  decls :: Port.VarMap -> Array String
  decls =
    SM.foldMap \k v ->
      [ declVar k $ quotes $ encodeURIComponent $ Port.renderVarMapValue v
      ]

eval :: forall e. Eval Query State Query (Slam e)
eval (Dismiss next) = pure next
eval (InitZClipboard code htmlEl next) = do
  let el = htmlElementToElement htmlEl
  liftEff $ Z.make el >>= Z.onCopy (Z.setData "text/plain" code)
  pure next
eval (SelectElement el next) = do
  liftEff $ select el
  pure next
