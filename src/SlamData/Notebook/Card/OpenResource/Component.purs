module SlamData.Notebook.Card.OpenResource.Component
  ( openResourceComponent
  , module SlamData.Notebook.Card.OpenResource.Component.Query
  , module SlamData.Notebook.Card.OpenResource.Component.State
  ) where

import SlamData.Prelude

import Data.Array as Arr
import Data.Argonaut (decodeJson, encodeJson)
import Data.Lens ((?~), (.~))
import Data.Path.Pathy (printPath, peel)

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B
import Halogen.HTML.Events.Indexed as HE
import Halogen.Component.Utils as HU

import SlamData.Effects (Slam)
import SlamData.FileSystem.Resource as R
import SlamData.Notebook.Card.CardType as CT
import SlamData.Notebook.Card.Component as NC
import SlamData.Notebook.Card.Common.EvalQuery as Eq
import SlamData.Notebook.Card.OpenResource.Component.Query (QueryP, Query(..))
import SlamData.Notebook.Card.OpenResource.Component.State (State, initialState, _selected, _browsing, _items, _loading)
import SlamData.Notebook.Card.Port as Port
import SlamData.Render.Common (glyph)
import SlamData.Render.CSS as Rc
import SlamData.Notebook.Card.Common.EvalQuery (runCardEvalT, liftWithCanceler')

import Utils.Path as Up

import Quasar.Aff as Quasar
import Quasar.Auth as Auth

type ORHTML = H.ComponentHTML QueryP
type ORDSL = H.ComponentDSL State QueryP Slam


openResourceComponent ∷ H.Component NC.CardStateP NC.CardQueryP Slam
openResourceComponent =
  NC.makeCardComponent
    { cardType: CT.OpenResource
    , component: H.lifecycleComponent
        { render
        , eval
        , initializer: Just (H.action (right ∘ Init))
        , finalizer: Nothing
        }
    , initialState: initialState
    , _State: NC._OpenResourceState
    , _Query: NC.makeQueryPrism NC._OpenResourceQuery
    }

render ∷ State → ORHTML
render state =
  HH.div
    [ HP.classes ([ Rc.openResourceCard ]
                  ⊕ (Rc.loading <$ guard state.loading))
    ]
    [ HH.div [ HP.classes [ Rc.openResourceCardMenu ] ]
      [ HH.button
          ([ HP.classes [ B.btn, B.btnDefault ] ]
             ⊕ case parentDir of
                Nothing →
                  [ HP.disabled true ]
                Just r →
                  [ HE.onClick (HE.input_ (right ∘ (ResourceSelected r))) ]
          )
          [ HH.text "Back" ]
      , HH.p_ [ HH.text selectedLabel ]
      ]
    , HH.div
      [ HP.classes [ B.listGroup
                   ]
      ]
      $ map renderItem state.items
    ]

  where
  selectedLabel ∷ String
  selectedLabel =
    fromMaybe ""
    $ map R.resourcePath state.selected
    <|> (pure $ printPath state.browsing)

  parentDir ∷ Maybe R.Resource
  parentDir = (R.Directory ∘ fst) <$> peel state.browsing

  renderItem ∷ R.Resource → ORHTML
  renderItem r =
    HH.div
      [ HP.classes ( [ B.listGroupItem ]
                     ⊕ ((guard (Just r ≡ state.selected)) $> B.active))
      , HE.onClick (HE.input_ (right ∘ (ResourceSelected r)))
      ]
      [ HH.a_
        [ glyphForResource r
        , HH.text $ R.resourceName r
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
    Just resource → do
      msg ←
        Quasar.messageIfResourceNotExists
          resource
          ("File " ⊕ R.resourcePath resource ⊕ " doesn't exist")
        # Auth.authed
        # liftWithCanceler'
      case msg of
        Nothing →
          pure $ k { output:
                       Just $ Port.TaggedResource { resource, tag: Nothing }
                   , messages: [ ]
                   }
        Just err →
          pure $ k { output: Just Port.Blocked
                   , messages: [ Left err ]
                   }
cardEval (Eq.NotifyRunCard next) = pure next
cardEval (Eq.Save k) = do
  mbRes ← H.gets _.selected
  k <$> case mbRes of
    Just res → pure $ encodeJson mbRes
    Nothing → do
      br ← H.gets _.browsing
      pure $ encodeJson $ R.Directory br
cardEval (Eq.Load js next) = do
  for_ (decodeJson js) resourceSelected
  pure next
cardEval (Eq.SetupCard info next) = pure next
cardEval (Eq.SetCanceler _ next) = pure next
cardEval (Eq.NotifyStopCard next) = pure next


openResourceEval ∷ Natural Query ORDSL
openResourceEval (ResourceSelected r next) = do
  loading ← H.gets _.loading
  when loading do
    HU.sendAfter zero (left $ Eq.NotifyStopCard unit)
    H.modify (_loading .~ false)
  resourceSelected r
  pure next
openResourceEval (Init next) = do
  updateItems *> rearrangeItems $> next

resourceSelected ∷ R.Resource → ORDSL Unit
resourceSelected r = do
  case R.getPath r of
    Left fp → do
      for_ (fst <$> peel fp) \dp → do
        oldBrowsing ← H.gets _.browsing
        unless (oldBrowsing ≡ dp) do
          H.modify (_browsing .~ dp)
          updateItems
      H.modify (_selected ?~ r)
    Right dp → do
      H.modify (_browsing .~ dp)
      H.modify (_selected .~ Nothing)
      updateItems
  rearrangeItems

updateItems ∷ ORDSL Unit
updateItems = do
  dp ← H.gets _.browsing
  H.modify (_loading .~ true)
  cs ←
    Quasar.children dp
      # Auth.authed
      # liftWithCanceler'
  mbSel ← H.gets _.selected
  H.modify (_items .~ cs)
  H.modify (_loading .~ false)

rearrangeItems ∷ ORDSL Unit
rearrangeItems = do
  mbSel ← H.gets _.selected
  for_ mbSel \sel → do
    is ← H.gets _.items
    let
      withoutSelected =
        Arr.filter (_ ≠ sel) is
      itemsToSet =
        Arr.cons sel $ Arr.sort withoutSelected
    H.modify (_items .~ itemsToSet)
--      HU.forceRerender
