{-
Copyright 2016 SlamData, Inc.

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

module SlamData.Notebook.Dialog.Embed.Component where

import SlamData.Prelude

import Control.UI.Browser (select)
import Control.UI.ZClipboard as Z

import Data.Foldable as F
import Data.String.Regex as Rx
import Data.StrMap as SM

import DOM.HTML.Types (HTMLElement, htmlElementToElement)

import Halogen as H
import Halogen.CustomProps as CP
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Renderer.String (renderHTML)
import Halogen.Themes.Bootstrap3 as B

import SlamData.Dialog.Render (modalDialog, modalHeader, modalBody, modalFooter)
import SlamData.Effects (Slam)
import SlamData.Notebook.Card.Port.VarMap as Port
import SlamData.Render.CSS as Rc

type State =
  { url :: String
  , varMap :: Port.VarMap
  }

data Query a
  = SelectElement HTMLElement a
  | InitZClipboard String (Maybe HTMLElement) a
  | Dismiss a

comp :: H.Component State Query Slam
comp = H.component { render, eval }

render :: State -> H.ComponentHTML Query
render { url, varMap } =
  modalDialog
    [ modalHeader "Embed card"
    , modalBody $
        HH.form
          [ CP.nonSubmit ]
          [ HH.div
              [ HP.classes [ B.formGroup ]
              , HE.onClick (\ev -> pure $ SelectElement ev.target unit)
              ]
              [ HH.textarea
                  [ HP.classes [ B.formControl, Rc.embedBox ]
                  , HP.readonly true
                  , HP.value code
                  ]
              ]
          ]
    , modalFooter
        [ HH.button
            [ HP.id_ "copy-button"
            , HP.classes [ B.btn, B.btnPrimary ]
            , HE.onClick (HE.input_ Dismiss)
            , HP.ref (H.action <<< InitZClipboard code)
            ]
            [ HH.text "Copy"
            ]
        ]
    ]
  where
  code :: String
  code =
    renderHTML $
      HH.script
        [ HP.mediaType { type: "text", subtype: "javascript", parameters: [] } ]
        [ HH.text javascriptCode ]

  statements :: Array String -> String
  statements = foldl (\m x -> m <> x <> ";\n") ""

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
      [ declVar k $ quotes $ Global.encodeURIComponent $ Port.renderVarMapValue v
      ]

eval :: Natural Query (H.ComponentDSL State Query Slam)
eval (Dismiss next) = pure next
eval (InitZClipboard code (Just htmlEl) next) = do
  let el = htmlElementToElement htmlEl
  H.fromEff $ Z.make el >>= Z.onCopy (Z.setData "text/plain" code)
  pure next
eval (InitZClipboard _ _ next) = pure next
eval (SelectElement el next) = H.fromEff (select el) $> next
