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
import Halogen.HTML.Events.Indexed as HE

import SlamData.Effects (Slam)
import SlamData.FileSystem.Resource as R
import SlamData.Notebook.Card.CardType as CT
import SlamData.Notebook.Card.Component as NC
import SlamData.Notebook.Card.Common.EvalQuery as Eq
import SlamData.Notebook.Card.OpenResource.Component.Query (QueryP, Query(..))
import SlamData.Notebook.Card.OpenResource.Component.State (State, initialState, _selected, _browsing, _items)
import SlamData.Notebook.Card.Port as Port
import SlamData.Render.Common (glyph)
import SlamData.Render.CSS as Rc
import SlamData.Notebook.Card.Common.EvalQuery (runCardEvalT, liftWithCanceler')


import Quasar.Aff as Quasar
import Quasar.Auth as Auth

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
      [ HP.classes [ B.listGroupItem ]
      , HE.onClick (HE.input_ (right ∘ (ResourceSelected r)))
      ]
      [ HH.a_
        [ glyphForResource r
        , HH.text $ R.resourcePath r
        ]
      ]
  glyphForResource ∷ R.Resource → ORHTML
  glyphForResource (R.File _) = glyph B.glyphiconFile
  glyphForResource (R.Notebook _) = glyph B.glyphiconBook
  glyphForResource (R.Directory _) = glyph B.glyphiconFolderOpen
  glyphForResource (R.Mount (R.Database _)) = glyph B.glyphiconHdd
  glyphForResource (R.Mount (R.View _)) = glyph B.glyphiconFile

eval ∷ Natural QueryP ORDSL
eval = coproduct cardEval openResourceEval


cardEval ∷ Natural Eq.CardEvalQuery ORDSL
cardEval (Eq.EvalCard info k) = do
  mbRes ← H.gets _.selected
  case mbRes of
    Nothing → pure $ k { output: Nothing, messages: [ ] }
    Just r → do
      msg ←
        Quasar.messageIfResourceNotExists
          resource
          ("File " ⊕ R.resourcePath resource ⊕ " doesn't exist")
        # Auth.authed
        # liftWithCanceler'
      case msg of
        Nothing →
          pure $ k { output:
                       Just Port.TaggedResource { resource: r, tag: Nothing }
                   , messages: [ ]
                   }
        Just err →
          pure $ k { output: Just Port.Blocked
                   , messages: [ Left err ]
                   }


  pure $ k { output: Nothing, messages: [ ] }
cardEval (Eq.NotifyRunCard next) = pure next
cardEval (Eq.Save k) = pure $ k $ encodeJson ""
cardEval (Eq.Load js next) = pure next
cardEval (Eq.SetupCard info next) = pure next
cardEval (Eq.SetCanceler _ next) = pure next

openResourceEval ∷ Natural Query ORDSL
openResourceEval (ResourceSelected r next) = do
  case R.getPath r of
    Left fp → modify (_selected ?~ fp)
    Right dp → do
      modify (_browsing .~ dp)
      modify (_selected .~ Nothing)

  pure next
