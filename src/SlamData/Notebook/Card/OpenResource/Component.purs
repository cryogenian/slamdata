module SlamData.Notebook.Card.OpenResource.Component
  ( openResourceComponent
  , module SlamData.Notebook.Card.OpenResource.Component.Query
  , module SlamData.Notebook.Card.OpenResource.Component.State
  ) where

import SlamData.Prelude

import Data.Argonaut (decodeJson, encodeJson)
import Data.Path.Pathy (printPath)

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.Effects (Slam)
import SlamData.FileSystem.Resource as R
import SlamData.Notebook.Card.CardType as CT
import SlamData.Notebook.Card.Component as NC
import SlamData.Notebook.Card.Common.EvalQuery as Eq
import SlamData.Notebook.Card.OpenResource.Component.Query (QueryP, Query(..))
import SlamData.Notebook.Card.OpenResource.Component.State (State, initialState)
import SlamData.Render.Common (glyph)
import SlamData.Render.CSS as Rc


type ORHTML = H.ComponentHTML QueryP
type ORDSL = H.ComponentDSL State QueryP Slam


openResourceComponent ∷ H.Component NC.CardStateP NC.CardQueryP Slam
openResourceComponent =
  NC.makeCardComponent
    { cardType: CT.OpenResource
    , component: H.component { render, eval }
    , initialState: initialState
    , _State: NC._OpenResourceState
    , _Query: NC.makeQueryPrism NC._OpenResourceQuery
    }

render ∷ State → ORHTML
render state =
  HH.div [ HP.classes [ Rc.openResourceCard ] ]
    [ HH.div [ HP.classes [ Rc.openResourceCardMenu ] ]
      [ HH.button [ HP.classes [ B.btn, B.btnDefault ] ]
          [ HH.text "Back" ]
      , HH.p_ [ HH.text $ printPath state.browsing ]
      ]
    , HH.div
      [ HP.classes [ B.listGroup
                   ]
      ]
      $ map renderItem state.items
    ]

  where
  renderItem ∷ R.Resource → ORHTML
  renderItem r =
    HH.div
      [ HP.classes [ B.listGroupItem ] ]
      [ HH.a_
        [ glyph B.glyphiconCutlery
        , HH.text $ R.resourcePath r
        ]
      ]

eval ∷ Natural QueryP ORDSL
eval = coproduct cardEval openResourceEval

cardEval ∷ Natural Eq.CardEvalQuery ORDSL
cardEval (Eq.EvalCard info k) =
  pure $ k { output: Nothing, messages: [ ] }
cardEval (Eq.NotifyRunCard next) = pure next
cardEval (Eq.Save k) = pure $ k $ encodeJson ""
cardEval (Eq.Load js next) = pure next
cardEval (Eq.SetupCard info next) = pure next
cardEval (Eq.SetCanceler _ next) = pure next

openResourceEval ∷ Natural Query ORDSL
openResourceEval (Empty next) = pure next
