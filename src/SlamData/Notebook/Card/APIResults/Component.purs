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

module SlamData.Notebook.Card.APIResults.Component where

import SlamData.Prelude

import Control.Monad.Error.Class as EC

import Data.Argonaut as J
import Data.Lens as Lens
import Data.StrMap as SM

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.Effects (Slam)
import SlamData.Notebook.Card.APIResults.Component.Query (QueryP)
import SlamData.Notebook.Card.APIResults.Component.State (State, initialState)
import SlamData.Notebook.Card.Common.EvalQuery (runCardEvalT)
import SlamData.Notebook.Card.Component as NC
import SlamData.Notebook.Card.Port as Port
import SlamData.Notebook.Card.CardType as Ct

type APIResultsDSL = H.ComponentDSL State QueryP Slam

apiResultsComponent :: H.Component NC.CardStateP NC.CardQueryP Slam
apiResultsComponent =
  NC.makeCardComponent
    { cardType: Ct.APIResults
    , component: H.component { render, eval }
    , initialState: initialState
    , _State: NC._APIResultsState
    , _Query: NC.makeQueryPrism NC._APIResultsQuery
    }

render :: State -> H.ComponentHTML QueryP
render { varMap } =
  HH.table
    [ HP.classes
        [ HH.className "form-builder"
        , B.table
        , B.tableStriped
        ]
    ]
    [ HH.thead_
        [ HH.tr_
            [ HH.th_ [ HH.text "Name" ]
            , HH.th_ [ HH.text "Value" ]
            ]
        ]
    , HH.tbody_ $ SM.foldMap renderItem varMap
    ]

  where
    renderItem :: String -> Port.VarMapValue -> Array (H.ComponentHTML QueryP)
    renderItem name val =
      [ HH.tr_
          [ HH.td_ [ HH.text name ]
          , HH.td_ [ HH.code_ [ HH.text $ Port.renderVarMapValue val ] ]
          ]
      ]

eval :: Natural QueryP APIResultsDSL
eval = coproduct evalCard (getConst >>> absurd)

evalCard :: Natural NC.CardEvalQuery APIResultsDSL
evalCard q =
  case q of
    NC.EvalCard info k ->
      k <$> runCardEvalT do
        case Lens.preview Port._VarMap =<< info.inputPort of
          Just varMap -> do
            lift $ H.modify (_ { varMap = varMap })
            pure $ Port.VarMap varMap
          Nothing -> EC.throwError "expected VarMap input"
    NC.SetupCard _ next -> pure next
    NC.NotifyRunCard next -> pure next
    NC.NotifyStopCard next -> pure next
    NC.Save k -> pure $ k J.jsonEmptyObject
    NC.Load json next -> pure next
    NC.SetCanceler _ next -> pure next
