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

module SlamData.Notebook.Cell.APIResults.Component where

import Prelude

import Control.Bind ((=<<))
import Control.Monad.Error.Class as EC
import Control.Monad.Trans as MT

import Data.Argonaut as J
import Data.Const
import Data.Functor.Coproduct
import Data.Lens as Lens
import Data.Maybe as M
import Data.StrMap as SM
import Data.Void as Void

import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as HP

import SlamData.Notebook.Cell.APIResults.Component.Query
import SlamData.Notebook.Cell.APIResults.Component.State
import SlamData.Notebook.Cell.Common.EvalQuery as NC
import SlamData.Notebook.Cell.Component as NC
import SlamData.Notebook.Cell.Port as Port
import SlamData.Effects (Slam())

type APIResultsDSL = ComponentDSL State QueryP Slam

apiResultsComponent :: Component NC.CellStateP NC.CellQueryP Slam
apiResultsComponent =
  NC.makeResultsCellComponent
    { component: component render eval
    , initialState: initialState
    , _State: NC._APIResultsState
    , _Query: NC.makeQueryPrism NC._APIResultsQuery
    }

render
  :: State
  -> ComponentHTML QueryP
render { varMap } =
  H.table [ HP.class_ $ H.className "form-builder" ] $
    [ H.thead_
        [ H.tr_
            [ H.th_ [ H.text "Name" ]
            , H.th_ [ H.text "Value" ]
            ]
        ]
    ] <> SM.foldMap renderItem varMap

  where
    renderItem :: String -> Port.VarMapValue -> Array (ComponentHTML QueryP)
    renderItem name val =
      [ H.tr_
          [ H.td_ [ H.text name ]
          , H.td_ [ H.code_ [ H.text $ Port.renderVarMapValue val ] ]
          ]
      ]

eval :: Natural QueryP APIResultsDSL
eval =
  coproduct
    evalCell
    (getConst >>> Void.absurd)

evalCell :: Natural NC.CellEvalQuery APIResultsDSL
evalCell q =
  case q of
    NC.EvalCell info k ->
      k <$> NC.runCellEvalT do
        case Lens.preview Port._VarMap =<< info.inputPort of
          M.Just varMap -> do
            MT.lift $ modify (_ { varMap = varMap })
            pure $ Port.VarMap varMap
          M.Nothing -> EC.throwError "expected VarMap input"
    NC.SetupCell _ next ->
      pure next
    NC.NotifyRunCell next ->
      pure next
    NC.Save k ->
      pure $ k J.jsonEmptyObject
    NC.Load json next ->
      pure next
    NC.SetCanceler _ next -> pure next
