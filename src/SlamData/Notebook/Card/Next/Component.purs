module SlamData.Notebook.Card.Next.Component
 ( nextCardComponent
 , module SlamData.Notebook.Card.Next.Component.State
 , module SlamData.Notebook.Card.Next.Component.Query
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

import SlamData.Effects (Slam)
import SlamData.Notebook.Card.CardType (cardName, cardGlyph, insertableCardTypes)
import SlamData.Notebook.Card.CardType as Ct
import SlamData.Notebook.Card.Common.EvalQuery as Ec
import SlamData.Notebook.Card.Component (makeCardComponent, makeQueryPrism, _NextState, _NextQuery)
import SlamData.Notebook.Card.Component as Cc
import SlamData.Notebook.Card.Next.Component.Query (QueryP, Query(AddCard, SetAvailableTypes, SetMessage), _AddCardType)
import SlamData.Notebook.Card.Next.Component.State (State, _message, _types, initialState)
import SlamData.Render.CSS as Rc

type NextHTML = H.ComponentHTML QueryP
type NextDSL = H.ComponentDSL State QueryP Slam

nextCardComponent :: Cc.CardComponent
nextCardComponent = makeCardComponent
  { cardType: Ct.NextAction
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
        ⊕ (map disabledButton $ insertableCardTypes Arr.\\ state.types))

    Just msg →
      HH.div [ HP.classes [ B.alert, B.alertInfo, Rc.nextActionCard ] ]
        [ HH.h4_ [ HH.text msg ] ]
  where
  cardTitle ∷ Ct.CardType → String
  cardTitle cty = "Insert " ⊕ cardName cty ⊕ " card"

  disabledTitle ∷ Ct.CardType → String
  disabledTitle cty = cardName cty ⊕ " is unavailable as next action"

  nextButton ∷ Ct.CardType → NextHTML
  nextButton cty =
    HH.li_
      [ HH.button
          [ HP.title $ cardTitle cty
          , ARIA.label $ cardTitle cty
          , HE.onClick (HE.input_ (right ∘ AddCard cty))
          ]
          [ HH.p_ [ HH.text (cardName cty) ]
          ]
      ]

  disabledButton ∷ Ct.CardType → NextHTML
  disabledButton cty =
    HH.li_
      [ HH.button
          [ HP.title $ disabledTitle cty
          , ARIA.label $ disabledTitle cty
          , HP.disabled true
          ]
          [ HH.p_ [ HH.text (cardName cty) ]
          ]
      ]

eval :: QueryP ~> NextDSL
eval = coproduct cardEval nextEval

cardEval :: Ec.CardEvalQuery ~> NextDSL
cardEval (Ec.EvalCard _ k) = map k ∘ Ec.runCardEvalT $ pure Nothing
cardEval (Ec.NotifyRunCard next) = pure next
cardEval (Ec.NotifyStopCard next) = pure next
cardEval (Ec.Save k) = pure $ k jsonEmptyObject
cardEval (Ec.Load _ next) = pure next
cardEval (Ec.SetupCard p next) = pure next
cardEval (Ec.SetCanceler _ next) = pure next

nextEval :: Query ~> NextDSL
nextEval (AddCard _ next) = pure next
nextEval (SetAvailableTypes cts next) = H.modify (_types .~ cts) $> next
nextEval (SetMessage mbTxt next) = H.modify (_message .~ mbTxt) $> next
