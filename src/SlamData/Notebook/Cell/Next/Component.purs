module SlamData.Notebook.Cell.Next.Component
 ( nextCellComponent
 , module SlamData.Notebook.Cell.Next.Component.State
 , module SlamData.Notebook.Cell.Next.Component.Query
 ) where

import SlamData.Prelude

import Data.Array as Arr
import Data.Argonaut (jsonEmptyObject)
import Data.Lens ((.~))

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Effects (Slam())
import SlamData.Notebook.Cell.CellType (cellName, cellGlyph, CellType(..), insertableCellTypes)
import SlamData.Notebook.Cell.Common.EvalQuery as Ec
import SlamData.Notebook.Cell.Component
  (makeCellComponent, makeQueryPrism, _NextState, _NextQuery)
import SlamData.Notebook.Cell.Component as Cc
import SlamData.Render.Common (row, glyph)
import SlamData.Render.CSS as Rc
import SlamData.Notebook.Cell.Next.Component.Query (QueryP, Query(AddCell, SetAvailableTypes, SetMessage), _AddCellType)
import SlamData.Notebook.Cell.Next.Component.State (State, _message, _types, initialState)
import SlamData.Notebook.Cell.CellType as Ct

type NextHTML = H.ComponentHTML QueryP
type NextDSL = H.ComponentDSL State QueryP Slam

nextCellComponent :: Cc.CellComponent
nextCellComponent = makeCellComponent
  { cellType: Ct.NextAction
  , component: H.component {render, eval}
  , initialState: initialState
  , _State: _NextState
  , _Query: makeQueryPrism _NextQuery
  }

render :: State → NextHTML
render state =
  case state.message of
    Nothing →
      HH.ul [ HP.classes [ Rc.nextActionCard ] ]
        (map nextButton state.types
        ⊕ (map disabledButton $ insertableCellTypes Arr.\\ state.types))

    Just msg →
      HH.div [ HP.classes [ B.alert, B.alertInfo, Rc.nextActionCard ] ]
        [ HH.h4_ [ HH.text msg ] ]
  where
  cardTitle ∷ Ct.CellType → String
  cardTitle cty = "Insert " ⊕ cellName cty ⊕ " card"

  disabledTitle ∷ Ct.CellType → String
  disabledTitle cty = cellName cty ⊕ " is unavailable as next action"

  nextButton ∷ Ct.CellType → NextHTML
  nextButton cty =
    HH.li_
      [ HH.button
          [ HP.title $ cardTitle cty
          , ARIA.label $ cardTitle cty
          , HE.onClick (HE.input_ (right ∘ AddCell cty))
          ]
          [ cellGlyph cty false
          , HH.p_ [ HH.text (cellName cty) ]
          ]
      ]

  disabledButton ∷ Ct.CellType → NextHTML
  disabledButton cty =
    HH.li_
      [ HH.button
          [ HP.title $ disabledTitle cty
          , ARIA.label $ disabledTitle cty
          , HP.disabled true
          ]
          [ cellGlyph cty true
          , HH.p_ [ HH.text (cellName cty) ]
          ]
      ]

eval :: Natural QueryP NextDSL
eval = coproduct cellEval nextEval

cellEval :: Natural Ec.CellEvalQuery NextDSL
cellEval (Ec.EvalCell _ k) = pure $ k { output: Nothing, messages: [ ]}
cellEval (Ec.NotifyRunCell next) = pure next
cellEval (Ec.Save k) = pure $ k jsonEmptyObject
cellEval (Ec.Load _ next) = pure next
cellEval (Ec.SetupCell p next) = pure next
cellEval (Ec.SetCanceler _ next) = pure next

nextEval :: Natural Query NextDSL
nextEval (AddCell _ next) = pure next
nextEval (SetAvailableTypes cts next) = H.modify (_types .~ cts) $> next
nextEval (SetMessage mbTxt next) = H.modify (_message .~ mbTxt) $> next
