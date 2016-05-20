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

module SlamData.Notebook.Cell.Download.Component where

import Prelude

import Control.MonadPlus (guard)
import Control.UI.Browser (encodeURIComponent)

import Data.Either (Either(..), isLeft, isRight, either)
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.Functor.Eff (liftEff)
import Data.Functor.Coproduct (coproduct, right)
import Data.Lens ((?~), (.~), (%~), _Left, _Right, preview)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Path.Pathy (printPath)
import Data.Tuple (Tuple(..))

import Halogen
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import Quasar.Aff (reqHeadersToJSON)
import Quasar.Auth as Auth
import Quasar.Paths as Paths

import SlamData.Download.Model as D
import SlamData.Download.Render as Rd
import SlamData.Effects (Slam())
import SlamData.FileSystem.Resource (resourcePath)
import SlamData.FileSystem.Resource as R
import SlamData.Notebook.Cell.CellType (cellName, cellGlyph, CellType(Download))
import SlamData.Notebook.Cell.Common.EvalQuery as Ec
import SlamData.Notebook.Cell.Component (makeSingularCellComponent, makeQueryPrism, _DownloadState, _DownloadQuery)
import SlamData.Notebook.Cell.Component as Cc
import SlamData.Notebook.Cell.Download.Component.Query
import SlamData.Notebook.Cell.Download.Component.State
import SlamData.Notebook.Cell.Port as P
import SlamData.Render.Common (row)
import SlamData.Render.CSS as Rc

type DownloadHTML = ComponentHTML QueryP
type DownloadDSL = ComponentDSL State QueryP Slam

downloadComponent :: Cc.CellComponent
downloadComponent = makeSingularCellComponent
  { name: cellName Download
  , glyph: cellGlyph Download
  , component: component render eval
  , initialState: initialState
  , _State: _DownloadState
  , _Query: makeQueryPrism _DownloadQuery
  }

render :: State -> DownloadHTML
render state =
  case state.source of
    Nothing ->
      H.div
        [ P.classes [ B.alert, B.alertDanger ] ]
        [ H.text "The current input cannot be downloaded" ]
    Just _ ->
      H.div
        [ P.classes [ Rc.downloadCellEditor ] ]
        [ renderDownloadTypeSelector state
        , renderDownloadConfiguration state
        ]

renderDownloadTypeSelector :: State -> DownloadHTML
renderDownloadTypeSelector state =
  H.div
    [ P.classes [ Rc.downloadTypeSelector ] ]
    [ H.img
        [ P.src "img/csv.svg"
        , P.classes (guard (isLeft state.options) $> B.active)
        , P.title "Comma separated values"
        , E.onClick (E.input_ (right <<< SetOutput D.CSV))
        ]
    , H.img
        [ P.src "img/json.svg"
        , P.classes (guard (isRight state.options) $> B.active)
        , P.title "JSON"
        , E.onClick (E.input_ (right <<< SetOutput D.JSON))
        ]
    ]

renderDownloadConfiguration  :: State -> DownloadHTML
renderDownloadConfiguration state =
  H.div
    [ P.classes [ Rc.downloadConfiguration ] ]
    $  [ either optionsCSV optionsJSON state.options]
    <> [ row
           [ H.div [ P.classes [ B.colXs4 ] ] [ compress state ]
           , H.div [ P.classes [ B.colXs8 ] ] [ downloadButton state ]
           ]
       ]

optionsCSV :: D.CSVOptions -> DownloadHTML
optionsCSV = Rd.optionsCSV (\lens v -> right <<< (ModifyCSVOpts (lens .~ v)))

optionsJSON :: D.JSONOptions -> DownloadHTML
optionsJSON = Rd.optionsJSON (\lens v -> right <<< (ModifyJSONOpts (lens .~ v)))

compress :: State -> DownloadHTML
compress state =
  H.div
    [ P.classes [ B.formGroup ] ]
    [ H.label_
        [ H.span_ [ H.text "Compress" ]
        , H.input
            [ P.inputType P.InputCheckbox
            , P.checked $ compressed state
            , P.enabled $ fromMaybe false (R.isFile <$> state.source)
            , E.onValueChange (E.input_ (right <<< ToggleCompress))
            ]
        ]
    ]
  where
  compressed :: State -> Boolean
  compressed state =
    fromMaybe false (not R.isFile <$> state.source) || state.compress

downloadButton :: State -> DownloadHTML
downloadButton state =
  H.a
    [ P.classes [ B.btn, B.btnPrimary ]
    , P.href $ fromMaybe "#" (url <$> state.source)
    , ARIA.label "Download"
    , P.title "Download"
    ]
    [ H.text "Download" ]
  where
  url :: R.Resource -> String
  url res =
    printPath Paths.dataUrl
      <> resourcePath res
      <> "?request-headers="
      <> headers

  headers :: String
  headers =
    encodeURIComponent
      $ show
      $ reqHeadersToJSON
      $ append state.authHeaders
      $ D.toHeaders' state

eval :: Natural QueryP DownloadDSL
eval = coproduct cellEval downloadEval

cellEval :: Natural Ec.CellEvalQuery DownloadDSL
cellEval (Ec.EvalCell { inputPort } continue) = do
  case inputPort of
    Just (P.TaggedResource { resource }) -> modify (_source ?~ resource)
    Just P.Blocked -> modify (_source .~ Nothing)
    _ -> pure unit
  pure $ continue { output: Nothing, messages: [] }
cellEval (Ec.NotifyRunCell next) = pure next
cellEval (Ec.Save k) = map (k <<< encode) get
cellEval (Ec.Load json next) = do
  for_ (decode json) set
  setAuthHeaders
  pure next
cellEval (Ec.SetupCell { inputPort } next) = do
  modify $ _source .~ preview P._Resource inputPort
  setAuthHeaders
  pure next
cellEval (Ec.SetCanceler _ next) = pure next

setAuthHeaders :: DownloadDSL Unit
setAuthHeaders = do
  hs <- liftEff Auth.authHeaders
  modify $ _authHeaders .~ hs

downloadEval :: Natural Query DownloadDSL
downloadEval (SetOutput ty next) = do
  options <- gets _.options
  case Tuple ty options of
    Tuple D.CSV (Right _) -> modify _{options = Left D.initialCSVOptions}
    Tuple D.JSON (Left _) -> modify _{options = Right D.initialJSONOptions}
    _ -> pure unit
  pure next
downloadEval (ModifyCSVOpts fn next) = modify (_options <<< _Left %~ fn) $> next
downloadEval (ModifyJSONOpts fn next) = modify (_options <<< _Right %~ fn) $> next
downloadEval (ToggleCompress next) = modify (_compress %~ not) $> next
