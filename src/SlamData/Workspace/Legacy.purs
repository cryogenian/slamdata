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

module SlamData.Workspace.Legacy where

import SlamData.Prelude
import Control.Monad.Eff.Exception as Exn
import Data.Array as Array
import Data.Map as Map
import Data.Path.Pathy as Pathy
import Quasar.Advanced.QuasarAF as QF
import Quasar.FS as QFS
import SlamData.Quasar.Data as Quasar
import SlamData.Quasar.Error as QE
import SlamData.Wiring.Cache as Cache
import SlamData.Workspace.Card.BuildChart.Gauge.Model as BuildGauge
import SlamData.Workspace.Card.BuildChart.Graph.Model as BuildGraph
import SlamData.Workspace.Card.BuildChart.Legacy as ChartLegacy
import SlamData.Workspace.Card.BuildChart.Metric.Model as BuildMetric
import SlamData.Workspace.Card.BuildChart.Sankey.Model as BuildSankey
import SlamData.Workspace.Card.CardId as CID
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Deck.DeckId as DID
import SlamData.Workspace.Model (Workspace, decode) as Current
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Future (defer, wait)
import Control.Monad.Fork (class MonadFork)
import Data.Argonaut (Json, decodeJson, (.?))
import Data.Path.Pathy ((</>))
import SlamData.Effects (SlamDataEffects)
import SlamData.Quasar.Class (class QuasarDSL, liftQuasar)
import SlamData.Workspace.Card.CardId (CardId)
import SlamData.Workspace.Card.Model (AnyCardModel(..))
import SlamData.Workspace.Deck.DeckId (DeckId)
import SlamData.Workspace.Deck.Model (Deck, emptyDeck) as Current
import Utils.Path (DirPath)

type Workspace =
  { root ∷ Maybe DeckId
  }

type Deck =
  { parent ∷ Maybe (DeckId × CardId)
  , mirror ∷ Array (DeckId × CardId)
  , cards ∷ Array Card
  , name ∷ String
  }

type Card =
  { cardId ∷ CID.CardId
  , model ∷ AnyCardModel
  }

data WorkspaceStatus = Legacy | Current

isLegacy ∷ WorkspaceStatus → Boolean
isLegacy Legacy = true
isLegacy _ = false

decodeWorkspace ∷ Json → Either String Workspace
decodeWorkspace = decodeJson >=> \obj ->
  { root: _
  } <$> obj .? "root"

decodeDeck ∷ Json → Either String Deck
decodeDeck = decodeJson >=> \obj → do
  version ← obj .? "version"
  unless (version ≡ 3) $ throwError "Expected deck format v3"
  parent ← obj .? "parent"
  mirror ← obj .? "mirror"
  cards ← traverse decodeCard =<< obj .? "cards"
  name ← obj .? "name" <|> pure ""
  pure { parent, mirror, cards, name }

decodeCard ∷ Json → Either String Card
decodeCard js = do
  obj ← decodeJson js
  cardId ← obj .? "cardId"
  cardTypeStr ← obj .? "cardType"
  modelJS ← obj .? "model"
  model ←
    if cardTypeStr ≡ "chart-options"
      then
        BuildMetric <$> BuildMetric.decode modelJS
        <|> BuildSankey <$> BuildSankey.decode modelJS
        <|> BuildGauge <$> BuildGauge.decode modelJS
        <|> BuildGraph <$> BuildGraph.decode modelJS
        <|> ChartLegacy.decode legacyConf modelJS
      else do
        cardType ← obj .? "cardType"
        Card.decodeCardModel cardType modelJS
  pure { cardId, model }
  where
  legacyConf =
    { pie: BuildPie
    , line: BuildLine
    , bar: BuildBar
    , area: BuildArea
    , scatter: BuildScatter
    , radar: BuildRadar
    , funnel: BuildFunnel
    , heatmap: BuildHeatmap
    , boxplot: BuildBoxplot
    }

loadGraph
  ∷ ∀ f m
  . ( MonadAff SlamDataEffects m
    , MonadFork Exn.Error m
    , Parallel f m
    , QuasarDSL m
    )
  ⇒ DirPath
  → Workspace
  → m (Either QE.QError Current.Workspace)
loadGraph path { root } = runExceptT do
  reqs ← Cache.make
  cardIdMap ← Cache.make
  cards ← Cache.make
  decks ← Cache.make

  let
    loadDeck ∷ DeckId → ExceptT QE.QError m Current.Deck
    loadDeck deckId = do
      pending ← Cache.get deckId reqs
      case pending of
        Just req → ExceptT $ wait req
        Nothing  → do
          req ← lift $ defer $ runExceptT do
            let deckPath = path </> Pathy.dir (DID.toString deckId) </> Pathy.file "index"
            deck ← ExceptT $ (_ >>= decodeDeck >>> lmap QE.msgToQError) <$> Quasar.load deckPath
            cardIds ← loadCards deckId deck
            let model = { name: deck.name, cards: cardIds }
            Cache.put deckId model decks
            pure model
          Cache.put deckId req reqs
          ExceptT $ wait req

    loadCards ∷ DeckId → Deck → ExceptT QE.QError m (Array CardId)
    loadCards deckId { mirror, cards: cardIds } = do
      parTraverse loadDeck (Array.nub (fst <$> mirror))
      mirrorIds ← traverse lookupCardId mirror
      cardIds' ← traverse (loadCard deckId) cardIds
      pure (mirrorIds <> cardIds')

    loadCard ∷ DeckId → Card → ExceptT QE.QError m CardId
    loadCard deckId { cardId, model } = do
      parTraverse loadDeck (Card.childDeckIds model)
      newId ← lift CID.make
      Cache.put (deckId × cardId) newId cardIdMap
      Cache.put newId model cards
      pure newId

    lookupCardId ∷ DeckId × CardId → ExceptT QE.QError m CardId
    lookupCardId coord = do
      cid ← Cache.get coord cardIdMap
      case cid of
        Nothing → QE.throw $ "Card id not found for coord: " <> show coord
        Just c  → pure c

  case root of
    Nothing → do
      rootId ← DID.make
      pure
        { rootId
        , decks: Map.singleton rootId Current.emptyDeck
        , cards: Map.empty ∷ Map.Map CardId AnyCardModel
        }
    Just rootId → do
      loadDeck rootId
      decks' ← Cache.snapshot decks
      cards' ← Cache.snapshot cards
      pure
        { rootId
        , decks: decks'
        , cards: cards'
        }

loadCompatWorkspace
  ∷ ∀ f m
  . ( MonadAff SlamDataEffects m
    , MonadFork Exn.Error m
    , Parallel f m
    , QuasarDSL m
    )
  ⇒ DirPath
  → m (Either QE.QError (WorkspaceStatus × Current.Workspace))
loadCompatWorkspace path = runExceptT do
  root ← ExceptT $ Quasar.load $ path </> Pathy.file "index"
  case Current.decode root, decodeWorkspace root of
    Right ws, _ → pure (Current × ws)
    _, Right ws → map (Legacy × _) $ ExceptT $ loadGraph path ws
    Left err, _ → QE.throw err

pruneLegacyData
  ∷ ∀ f m
  . ( Functor m
    , Parallel f m
    , QuasarDSL m
    )
  ⇒ DirPath
  → m (Either QE.QError Unit)
pruneLegacyData path = runExceptT do
  children ← ExceptT $ liftQuasar (QF.dirMetadata path)
  let
    tmpDir = path </> Pathy.dir ".tmp"
    deckDirs = flip foldMap children case _ of
      QFS.Directory d → fromMaybe [] do
        name ← Pathy.runDirName <$> Pathy.dirName d
        deckId ← DID.fromString name
        pure [d]
      _ → []
  void $ parTraverse (ExceptT ∘ Quasar.delete) $
    Left <$> (pure tmpDir <> deckDirs)
