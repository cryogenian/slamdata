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

module SlamData.Workspace.Eval.Persistence where

import SlamData.Prelude

import Control.Monad.Aff.AVar (AVar, takeVar, putVar, modifyVar, killVar)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Aff.Future (wait, defer)
import Control.Monad.Eff.Exception as Exn
import Control.Monad.Fork (class MonadFork, fork)
import Control.Parallel (parSequence)

import Data.Array as Array
import Data.List (List(..), (:))
import Data.List as List
import Data.Map as Map
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as Pathy
import Data.Rational ((%))

import SlamData.Effects (SlamDataEffects)
import SlamData.Quasar.Data as Quasar
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Error as QE
import SlamData.Workspace.AccessType (isEditable)
import SlamData.Workspace.Card.CardId as CID
import SlamData.Workspace.Card.Model as CM
import SlamData.Workspace.Card.Port (Port)
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Draftboard.Layout as Layout
import SlamData.Workspace.Card.Draftboard.Pane as Pane
import SlamData.Workspace.Card.Draftboard.Orientation as Orn
import SlamData.Workspace.Deck.DeckId as DID
import SlamData.Workspace.Deck.Model as DM
import SlamData.Workspace.Eval as Eval
import SlamData.Workspace.Eval.Card as Card
import SlamData.Workspace.Eval.Deck as Deck
import SlamData.Workspace.Eval.Graph (unfoldGraph, EvalGraph)
import SlamData.Workspace.Model as WM
import SlamData.Wiring (Wiring)
import SlamData.Wiring as Wiring
import SlamData.Wiring.Cache as Cache

import Utils (censor)
import Utils.Aff (laterVar)

defaultSaveDebounce ∷ Int
defaultSaveDebounce = 500

defaultEvalDebounce ∷ Int
defaultEvalDebounce = 500

type Persist f m a =
  ( MonadAff SlamDataEffects m
  , MonadAsk Wiring m
  , MonadFork Exn.Error m
  , Parallel f m
  , QuasarDSL m
  ) ⇒ a

putDeck ∷ ∀ f m. Persist f m (Deck.Id → Deck.Model → m (Either QE.QError Unit))
putDeck deckId deck = do
  { path, eval, accessType } ← Wiring.expose
  ref ← defer do
    res ←
      if isEditable accessType
        then Quasar.save (Deck.deckIndex path deckId) $ Deck.encode deck
        else pure (Right unit)
    pure $ res $> deck
  Cache.alter deckId (updateOrFork ref) eval.decks
  rmap (const unit) <$> wait ref
  where
    updateOrFork ref = case _ of
      Just cell →
        pure $ Just cell
          { value = ref
          , model = deck
          }
      Nothing → do
        cell ← { value: ref, model: deck, bus: _ } <$> liftAff Bus.make
        fork do
          populateCards deckId deck
          forkDeckProcess deckId cell.bus
        pure (Just cell)

saveDeck ∷ ∀ f m. Persist f m (Deck.Id → m Unit)
saveDeck deckId = do
  { eval } ← Wiring.expose
  newDeck ← runMaybeT do
    cell ← MaybeT $ Cache.get deckId eval.decks
    deck ← MaybeT $ censor <$> wait cell.value
    cards ← MaybeT $ sequence <$> traverse getCard (Tuple deckId ∘ _.cardId <$> deck.cards)
    pure deck { cards = (\c → c.value.model) <$> cards }
  for_ newDeck (void ∘ putDeck deckId)

-- | Loads a deck from a DeckId. Returns the model.
getDeck ∷ ∀ f m. Persist f m (Deck.Id → m (Either QE.QError Deck.Model))
getDeck =
  getDeck' >=> _.value >>> wait

-- | Loads a deck from a DeckId. This has the effect of loading decks from
-- | which it extends (for mirroring) and populating the card graph. Returns
-- | the "cell" (model promise paired with its message bus).
getDeck' ∷ ∀ f m. Persist f m (Deck.Id → m Deck.Cell)
getDeck' deckId = do
  { path, eval } ← Wiring.expose
  let
    cacheVar = Cache.unCache eval.decks
  decks ← liftAff (takeVar cacheVar)
  case Map.lookup deckId decks of
    Just cell → do
      liftAff $ putVar cacheVar decks
      pure cell
    Nothing → do
      value ← defer do
        let
          deckPath = Deck.deckIndex path deckId
        result ← runExceptT do
          deck ← ExceptT $ (_ >>= Deck.decode >>> lmap QE.msgToQError) <$> Quasar.load deckPath
          _    ← ExceptT $ populateCards deckId deck
          pure deck
        case result of
          Left _ →
            liftAff $ modifyVar (Map.delete deckId) cacheVar
          Right model →
            Cache.alter deckId (pure ∘ map (_ { model = model })) eval.decks
        pure result
      cell ← { value, model: Deck.emptyDeck, bus: _ } <$> liftAff Bus.make
      liftAff do
        putVar cacheVar (Map.insert deckId cell decks)
      forkDeckProcess deckId cell.bus
      pure cell

-- | Populates the card eval graph based on a deck model. This may fail as it
-- | also attempts to load/hydrate foreign cards (mirrors) as well.
populateCards ∷ ∀ f m. Persist f m (Deck.Id → Deck.Model → m (Either QE.QError Unit))
populateCards deckId deck = runExceptT do
  { eval } ← Wiring.expose
  decks ←
    ExceptT $ sequence <$>
      parTraverse getDeck (Array.nub (fst <$> deck.mirror))
  let
    cards = List.fromFoldable deck.cards
  childDecks ←
    ExceptT $ sequence <$>
      parTraverse getDeck (List.nub (CM.childDeckIds ∘ _.model =<< cards))
  case Array.last deck.mirror of
    Nothing → lift $ threadCards eval.cards cards
    Just coord → do
      cell ← do
        mb ← Cache.get coord eval.cards
        case mb of
          Nothing → QE.throw ("Card not found in eval cache: " <> show coord)
          Just a  → pure a
      let
        next = case List.head cards of
          Nothing → Left deckId
          Just c  → Right (deckId × c.cardId)
        cell' = cell { next = next : cell.next }
      Cache.put coord cell' eval.cards
      lift $ threadCards eval.cards cards
  where
    threadCards cache = case _ of
      c : Nil →
        makeCell c Nil cache
      c : c' : cs → do
        makeCell c (pure c'.cardId) cache
        threadCards cache (c' : cs)
      Nil →
        pure unit

    makeCell card next cache = do
      let
        coord = deckId × card.cardId
      cell ← makeCardCell card Nothing (Right ∘ Tuple deckId <$> next)
      Cache.put coord cell cache
      forkCardProcess coord cell.bus

forkDeckProcess ∷ ∀ f m . Persist f m (Deck.Id → Bus.BusRW Deck.EvalMessage → m Unit)
forkDeckProcess deckId = forkLoop case _ of
  Deck.ParentChange parent → do
    { eval } ← Wiring.expose
    deckCell ← getDeck' deckId
    mbDeck ← wait deckCell.value
    for_ mbDeck \deck → do
      let
        deck' = deck { parent = parent }
        value' = pure (Right deck')
      Cache.put deckId (deckCell { value = value' }) eval.decks
      queueSaveDefault deckId
  _ →
    pure unit

forkCardProcess ∷ ∀ f m. Persist f m (Card.Coord → Bus.BusRW Card.EvalMessage → m Unit)
forkCardProcess coord@(deckId × cardId) = forkLoop case _ of
  Card.ModelChange source model → do
    { eval } ← Wiring.expose
    Cache.alter coord (pure ∘ map (updateCellModel model)) eval.cards
    mbGraph ← snapshotGraph coord
    for_ mbGraph \graph → do
      queueSaveDefault deckId
      queueEval' defaultEvalDebounce source graph
      deck ← getDeck' deckId
      liftAff $ Bus.write (Deck.CardChange coord) deck.bus
  _ →
    pure unit

snapshotGraph ∷ ∀ f m. Persist f m (Card.Coord → m (Maybe EvalGraph))
snapshotGraph coord = do
  { eval } ← Wiring.expose
  unfoldGraph
    <$> Cache.snapshot eval.cards
    <*> Cache.snapshot eval.decks
    <*> pure coord

queueSave ∷ ∀ f m. Persist f m (Int → Deck.Id → m Unit)
queueSave ms deckId = do
  { eval } ← Wiring.expose
  debounce ms deckId { avar: _ } eval.pendingSaves (pure unit) do
    saveDeck deckId

queueSaveImmediate ∷ ∀ f m. Persist f m (Deck.Id → m Unit)
queueSaveImmediate = queueSave 0

queueSaveDefault ∷ ∀ f m. Persist f m (Deck.Id → m Unit)
queueSaveDefault = queueSave defaultSaveDebounce

queueEval' ∷ ∀ f m. Persist f m (Int → Card.DisplayCoord → EvalGraph → m Unit)
queueEval' ms source@(_ × coord) graph = do
  { eval } ← Wiring.expose
  let
    pending =
      { source
      , graph
      , avar: _
      }
  debounce ms coord pending eval.pendingEvals
    (Eval.notifyDecks (Deck.Pending coord) graph)
    (Eval.evalGraph source graph)

queueEval ∷ ∀ f m. Persist f m (Int → Card.DisplayCoord → m Unit)
queueEval ms source@(_ × coord) = do
  _ ← pure unit
  traverse_ (queueEval' ms source) =<< snapshotGraph coord

queueEvalImmediate ∷ ∀ f m. Persist f m (Card.DisplayCoord → m Unit)
queueEvalImmediate = queueEval 0

queueEvalDefault ∷ ∀ f m. Persist f m (Card.DisplayCoord → m Unit)
queueEvalDefault = queueEval defaultEvalDebounce

queueEvalForDeck ∷ ∀ f m. Persist f m (Int → Deck.Id → m Unit)
queueEvalForDeck ms deckId =
  getDeck deckId >>= traverse_ \deck →
    for_ (Array.head (Deck.cardCoords deckId deck)) \coord →
      queueEval ms (Card.toAll coord)

freshWorkspace ∷ ∀ f m. Persist f m (Array CM.AnyCardModel → m (Deck.Id × Deck.Cell))
freshWorkspace anyCards = do
  { eval } ← Wiring.expose
  cards ← traverse freshCard anyCards
  bus ← liftAff Bus.make
  let
    deck = Deck.emptyDeck { cards = cards }
    value = pure (Right deck)
    cell = { bus, value, model: deck }
  rootId ← liftAff DID.make
  Cache.put rootId cell eval.decks
  forkDeckProcess rootId bus
  populateCards rootId deck
  pure (rootId × cell)
  where
    freshCard model = do
      cardId ← liftAff CID.make
      pure { cardId, model }

freshDeck ∷ ∀ f m. Persist f m (Maybe Card.Coord → m (Either QE.QError Deck.Id))
freshDeck parent = runExceptT do
  deckId ← liftAff DID.make
  ExceptT $ putDeck deckId $ Deck.emptyDeck { parent = parent }
  pure deckId

deleteDeck ∷ ∀ f m. Persist f m (Deck.Id → m (Either QE.QError (Maybe Deck.Id)))
deleteDeck deckId = runExceptT do
  { path, eval } ← Wiring.expose
  deck ← ExceptT $ getDeck deckId
  case deck.parent of
    Just _ →
      void $ parTraverse ExceptT
        [ Quasar.delete (Left (path </> Pathy.dir (DID.toString deckId)))
        , updateParentPointer deckId Nothing deck.parent
        ]
    Nothing →
      ExceptT $ Quasar.delete (Left path)
  Cache.remove deckId eval.decks
  pure (fst <$> deck.parent)

wrapDeck ∷ ∀ f m. Persist f m (Deck.Id → m (Either QE.QError Deck.Id))
wrapDeck deckId = runExceptT do
  cell ← lift $ getDeck' deckId
  deck ← ExceptT $ wait cell.value
  parentCoord ← Tuple <$> liftAff DID.make <*> liftAff CID.make
  let
    parentDeck = DM.wrappedDeck deck.parent (snd parentCoord) deckId
  ExceptT $ putDeck (fst parentCoord) parentDeck
  ExceptT $ updateParentPointer deckId (Just (fst parentCoord)) deck.parent
  liftAff $ Bus.write (Deck.ParentChange (Just parentCoord)) cell.bus
  pure (fst parentCoord)

wrapAndMirrorDeck ∷ ∀ f m. Persist f m (Deck.Id → m (Either QE.QError Deck.Id))
wrapAndMirrorDeck deckId = runExceptT do
  cell ← lift $ getDeck' deckId
  deck ← ExceptT $ wait cell.value
  newSharedId ← liftAff DID.make
  newMirrorId ← liftAff DID.make
  parentCoord ← Tuple <$> liftAff DID.make <*> liftAff CID.make
  let
    wrappedDeck =
      DM.splitDeck deck.parent (snd parentCoord) Orn.Vertical
        (List.fromFoldable [ deckId, newMirrorId ])
  if Array.null deck.cards
    then do
      let
        mirrored = deck { parent = Just parentCoord }
      parSequence
        [ ExceptT $ putDeck deckId mirrored
        , ExceptT $ putDeck newMirrorId (mirrored { name = "" })
        ]
    else do
      let
        oldCardIds = Tuple deckId ∘ _.cardId <$> deck.cards
        newCardIds = Tuple newSharedId ∘ snd <$> oldCardIds
        mirrored = deck
          { parent = Just parentCoord
          , mirror = deck.mirror <> newCardIds
          , cards = []
          , name = deck.name
          }
      parSequence
        [ ExceptT $ putDeck deckId mirrored
        , ExceptT $ putDeck newMirrorId (mirrored { name = "" })
        , ExceptT $ putDeck newSharedId (deck { name = "" })
        ]
  parSequence
    [ ExceptT $ putDeck (fst parentCoord) wrappedDeck
    , ExceptT $ updateParentPointer deckId (Just (fst parentCoord)) deck.parent
    ]
  lift $ relocateCardsTo newSharedId deckId (_.cardId <$> deck.cards)
  lift $ cloneActiveStateTo newMirrorId deckId
  pure (fst parentCoord)

mirrorDeck ∷ ∀ f m. Persist f m (Deck.Id → Card.Coord → m (Either QE.QError Deck.Id))
mirrorDeck childId coord = runExceptT do
  { eval } ← Wiring.expose
  deck ← ExceptT $ getDeck childId
  parent ← liftWithErr "Parent not found." $ getCard coord
  newSharedId ← liftAff DID.make
  newMirrorId ← liftAff DID.make
  ExceptT $ map sequence
    if Array.null deck.cards
      then do
        let
          mirrored = deck { name = "" }
        parSequence
          [ putDeck newMirrorId mirrored
          ]
      else do
        let
          oldCardIds = Tuple childId ∘ _.cardId <$> deck.cards
          newCardIds = Tuple newSharedId ∘ snd <$> oldCardIds
          mirrored = deck
            { parent = Just coord
            , mirror = deck.mirror <> newCardIds
            , cards = []
            , name = deck.name
            }
        parSequence
          [ putDeck childId mirrored
          , putDeck newMirrorId (mirrored { name = "" })
          , putDeck newSharedId (deck { name = "" })
          ]
  lift $ relocateCardsTo newSharedId childId (_.cardId <$> deck.cards)
  lift $ cloneActiveStateTo newMirrorId childId
  parent'' ← liftWithErr "Cannot mirror deck." $ pure $ mirrorInLayout newMirrorId parent.value.model.model
  liftAff $ Bus.write (Card.ModelChange (Card.toAll coord) parent'') parent.bus
  pure newSharedId
  where
  mirrorInLayout newId = case _ of
    CM.Draftboard { layout } → do
      cursor ← Pane.getCursorFor (Just childId) layout
      let
        cursor' = fromMaybe Nil (List.tail cursor)
        orn =
          case Pane.getAt cursor' layout of
            Just (Pane.Split o _) → o
            _ → Orn.Vertical
      layout' ←
        Layout.insertSplit
          (Pane.Cell (Just newId)) orn (1%2) Layout.SideB cursor' layout
      pure (CM.Draftboard { layout: layout' })
    _ →
      Nothing

unwrapDeck ∷ ∀ f m. Persist f m (Deck.Id → m (Either QE.QError Deck.Id))
unwrapDeck deckId = runExceptT do
  deck ← ExceptT $ getDeck deckId
  cards ← liftWithErr "Cards not found." $ getCards (Deck.cardCoords deckId deck)
  childId ← liftWithErr "Cannot unwrap deck." $ pure $ immediateChild (_.value.model.model ∘ snd <$> cards)
  cell ← lift $ getDeck' childId
  ExceptT $ updateParentPointer deckId (Just childId) deck.parent
  liftAff $ Bus.write (Deck.ParentChange deck.parent) cell.bus
  pure childId
  where
    immediateChild = case _ of
      [ model ] →
        case CM.childDeckIds model of
          childId : Nil → Just childId
          _ → Nothing
      _ → Nothing

collapseDeck ∷ ∀ f m. Persist f m (Deck.Id → Card.Coord → m (Either QE.QError Unit))
collapseDeck childId coord = runExceptT do
  { eval } ← Wiring.expose
  deck ← ExceptT $ getDeck childId
  cards ← liftWithErr "Cards not found." $ getCards (Deck.cardCoords childId deck)
  parent ← liftWithErr "Parent not found." $ getCard coord
  let
    parent' = parent.value.model.model
    cards' = _.value.model.model ∘ snd <$> cards
  childIds × parent'' ← liftWithErr "Cannot collapse deck." $ pure $ collapsed parent' cards'
  childDecks ← lift $ traverse getDeck' childIds
  liftAff do
    for_ childDecks \{ bus } →
      Bus.write (Deck.ParentChange (Just coord)) bus
    Bus.write (Card.ModelChange (Card.toAll coord) parent'') parent.bus
  where
    collapsed parent child = case parent, child of
      CM.Draftboard { layout }, [ cm@CM.Draftboard { layout: subLayout } ] → do
        cursor ← Pane.getCursorFor (Just childId) layout
        layout' ← Pane.modifyAt (const subLayout) cursor layout
        let
          childIds = CM.childDeckIds cm
          parent' = CM.Draftboard { layout: layout' }
        pure (childIds × parent')
      _, _ →
        Nothing

wrapAndGroupDeck ∷ ∀ f m. Persist f m (Orn.Orientation → Layout.SplitBias → Deck.Id → Deck.Id → m (Either QE.QError Unit))
wrapAndGroupDeck orn bias deckId newParentId = runExceptT do
  cell ← lift $ getDeck' deckId
  oldParentCoord ← liftWithErr "Parent not found." $ pure $ cell.model.parent
  oldParent ← liftWithErr "Parent not found." $ getCard oldParentCoord
  newParent ← lift $ getDeck' newParentId
  case oldParent.value.model.model of
    CM.Draftboard { layout } → do
      wrappedCoord ← Tuple <$> liftAff DID.make <*> liftAff CID.make
      let
        splits = case bias of
          Layout.SideA → [ deckId, newParentId ]
          Layout.SideB → [ newParentId, deckId ]
        wrappedDeck =
          DM.splitDeck cell.model.parent (snd wrappedCoord) orn
            (List.fromFoldable splits)
        layout' = layout <#> case _ of
          Just did | did ≡ deckId → Nothing
          Just did | did ≡ newParentId → Just (fst wrappedCoord)
          a → a
        parent' = CM.Draftboard { layout: layout' }
      ExceptT $ putDeck (fst wrappedCoord) wrappedDeck
      liftAff do
        Bus.write (Deck.ParentChange (Just wrappedCoord)) cell.bus
        Bus.write (Deck.ParentChange (Just wrappedCoord)) newParent.bus
        Bus.write (Card.ModelChange (Card.toAll oldParentCoord) parent') oldParent.bus
    _ → do
      QE.throw "Could not group deck."

groupDeck ∷ ∀ f m. Persist f m (Orn.Orientation → Layout.SplitBias → Deck.Id → Deck.Id → Card.Coord → m (Either QE.QError Unit))
groupDeck orn bias deckId newParentId newParentCoord = runExceptT do
  cell ← lift $ getDeck' deckId
  oldParentCoord ← liftWithErr "Parent not found." $ pure $ cell.model.parent
  oldParent ← liftWithErr "Parent not found." $ getCard oldParentCoord
  newParent ← liftWithErr "Destination not found." $ getCard newParentCoord

  case oldParent.value.model.model
     , newParent.value.model.model of
    CM.Draftboard { layout }, CM.Draftboard { layout: inner } → do
      let
        inner' =
          Layout.insertRootSplit (Pane.Cell (Just deckId)) orn (1%2) bias layout
        layout' = layout <#> case _ of
          Just did | did ≡ deckId → Nothing
          a → a
        child' = CM.Draftboard { layout: inner' }
        parent' = CM.Draftboard { layout: layout' }
      liftAff do
        Bus.write (Deck.ParentChange (Just newParentCoord)) cell.bus
        Bus.write (Card.ModelChange (Card.toAll newParentCoord) child') newParent.bus
        Bus.write (Card.ModelChange (Card.toAll oldParentCoord) parent') oldParent.bus
    _, _ →
      ExceptT $ wrapAndGroupDeck orn bias deckId newParentId

renameDeck ∷ ∀ f m. Persist f m (Deck.Id → String → m (Either QE.QError Unit))
renameDeck deckId name = runExceptT do
  cell ← lift $ getDeck' deckId
  deck ← ExceptT $ wait cell.value
  ExceptT $ putDeck deckId (deck { name = name })
  liftAff $ Bus.write (Deck.NameChange name) cell.bus
  pure unit

addCard ∷ ∀ f m. Persist f m (Deck.Id → CT.CardType → m (Either QE.QError Card.Coord))
addCard deckId cty = runExceptT do
  { eval } ← Wiring.expose
  deckCell ← lift $ getDeck' deckId
  deck ← ExceptT $ wait deckCell.value
  cardId ← liftAff CID.make
  let
    coord = deckId × cardId
  lift do
    input ← runMaybeT do
      last ← MaybeT $ pure $ Array.last (Deck.cardCoords deckId deck)
      cell ← MaybeT $ Cache.get last eval.cards
      let
        next' = List.filter (not ∘ eq (Left deckId)) cell.next
        cell' = cell { next = Right coord : next' }
      lift $ Cache.put last cell' eval.cards
      MaybeT $ pure $ cell.value.output
    let
      card = { cardId, model: Card.cardModelOfType (fromMaybe Port.Initial input) cty }
      deck' = deck { cards = Array.snoc deck.cards card }
      value' = pure (Right deck')
    cell ← makeCardCell card input mempty
    Cache.put coord cell eval.cards
    Cache.put deckId (deckCell { value = value' }) eval.decks
    liftAff $ Bus.write (Deck.CardChange coord) deckCell.bus
    forkCardProcess coord cell.bus
    queueSaveDefault deckId
    queueEvalImmediate (Card.toAll coord)
    pure coord

removeCard ∷ ∀ f m. Persist f m (Deck.Id → Card.Coord → m (Either QE.QError Unit))
removeCard deckId coord@(deckId' × cardId) = runExceptT do
  { eval } ← Wiring.expose
  deckCell ← lift $ getDeck' deckId
  deck ← ExceptT $ wait deckCell.value
  lift do
    let
      coords = Array.span (not ∘ eq coord) (Deck.cardCoords deckId deck)
      deck' =
        if deckId ≡ deckId'
          then deck { cards = Array.takeWhile (not ∘ eq cardId ∘ _.cardId) deck.cards }
          else deck { cards = [], mirror = Array.takeWhile (not ∘ eq coord) deck.mirror }
      value' = pure (Right deck')
    output ← runMaybeT do
      last ← MaybeT $ pure $ Array.last coords.init
      cell ← MaybeT $ Cache.get last eval.cards
      let
        leaf = if Array.null deck'.cards then pure (Left deckId) else mempty
        next' = List.delete (Right coord) cell.next
        cell' = cell { next = leaf <> next' }
      lift $ Cache.put last cell' eval.cards
      MaybeT $ pure $ cell.value.output
    Cache.put deckId (deckCell { value = value' }) eval.decks
    queueSaveDefault deckId
    liftAff $ Bus.write (Deck.Complete coords.init (fromMaybe Port.Initial output)) deckCell.bus

updateParentPointer ∷ ∀ f m. Persist f m (Deck.Id → Maybe Deck.Id → Maybe Card.Coord → m (Either QE.QError Unit))
updateParentPointer oldId newId parent = runExceptT case parent of
  Just coord@(deckId × cardId) → do
    -- We need to guard on the parentDeck because it may not be loaded if we
    -- are viewing the child at the root of the UI.
    parentDeck ← ExceptT $ getDeck deckId
    cell ← getCard coord
    for_ cell \{ bus, value } → do
      let
        model' = CM.updatePointer oldId newId value.model.model
        message = Card.ModelChange (Card.toAll coord) model'
      liftAff $ Bus.write message bus
  Nothing → do
    { path } ← Wiring.expose
    res ← ExceptT $ map sequence $ traverse (WM.setRoot (path </> Pathy.file "index")) newId
    pure unit

relocateCardsTo ∷ ∀ f m. Persist f m (Deck.Id → Deck.Id → Array Card.Id → m Unit)
relocateCardsTo deckId oldDeckId cardIds = do
  { eval } ← Wiring.expose
  let
    oldCoords = Tuple oldDeckId <$> cardIds
    newCoords = Tuple deckId <$> cardIds
  mbCards ← getCards oldCoords
  for_ mbCards \cards → do
    go eval.cards (List.fromFoldable (snd <$> cards))
  where
    go cache = case _ of
      c : Nil → do
        let
          cardId = c.value.model.cardId
          coord = deckId × cardId
          next = Left oldDeckId : c.next
        liftAff $ Bus.kill (Exn.error "Relocated") c.bus
        Cache.remove (oldDeckId × cardId) cache
        Cache.alter coord
          (pure ∘ map \c' → c' { value = c.value, next = Left oldDeckId : c'.next })
          cache
      c1 : c2 : cs → do
        let
          cardId1 = c1.value.model.cardId
          cardId2 = c2.value.model.cardId
          coord2 = oldDeckId × cardId2
          coord = deckId × cardId1
          updateNext = map (map \c → if c ≡ coord2 then deckId × cardId2 else c)
        liftAff $ Bus.kill (Exn.error "Relocated") c1.bus
        Cache.remove (oldDeckId × cardId1) cache
        Cache.alter coord
          (pure ∘ map \c' → c' { value = c1.value, next = updateNext c'.next })
          cache
        go cache (c2 : cs)
      Nil →
        pure unit

cloneActiveStateTo ∷ ∀ f m. Persist f m (Deck.Id → Deck.Id → m Unit)
cloneActiveStateTo to from = do
  { cache } ← Wiring.expose
  activeState ← Cache.get from cache.activeState
  for_ activeState \as → Cache.put to as cache.activeState

forkLoop
  ∷ ∀ m r a
  . ( MonadAff SlamDataEffects m
    , MonadFork Exn.Error m
    )
  ⇒ (a → m Unit)
  → Bus.BusR' r a
  → m Unit
forkLoop handler bus = void (fork loop)
  where
    loop = do
      msg ← liftAff (Bus.read bus)
      fork (handler msg)
      loop

makeCardCell
  ∷ ∀ m
  . ( MonadAff SlamDataEffects m
    , Monad m
    )
  ⇒ Card.Model
  → Maybe Port
  → List (Either Deck.Id Card.Coord)
  → m Card.Cell
makeCardCell model input next = do
  let
    value ∷ Card.EvalResult
    value =
      { model
      , input
      , output: Nothing
      , state: Nothing
      , sources: mempty
      , tick: Nothing
      }
  bus ← liftAff Bus.make
  pure { bus, next, value }

getCard
  ∷ ∀ m
  . ( MonadAff SlamDataEffects m
    , MonadAsk Wiring m
    )
  ⇒ Card.Coord
  → m (Maybe Card.Cell)
getCard coord = do
  { eval } ← Wiring.expose
  Cache.get coord eval.cards

getCards
  ∷ ∀ m
  . ( MonadAff SlamDataEffects m
    , MonadAsk Wiring m
    )
  ⇒ Array Card.Coord
  → m (Maybe (Array (Deck.Id × Card.Cell)))
getCards = map sequence ∘ traverse go
  where
    go coord = map (fst coord × _) <$> getCard coord

getEvaluatedCards
  ∷ ∀ m
  . ( MonadAff SlamDataEffects m
    , MonadAsk Wiring m
    )
  ⇒ Array Card.Coord
  → m (Maybe (Array (Deck.Id × Card.Cell) × Port))
getEvaluatedCards = map (flip bind (Array.foldM go (mempty × Port.Initial))) ∘ getCards
  where
    go acc@(cs × Card.CardError _) _ = pure acc
    go acc@(cs × _) card@(deckId × cell@{ value: { tick: Just _, output: Just out }}) =
      pure (Array.snoc cs card × out)
    go _ _ = Nothing

debounce
  ∷ ∀ k m r
  . ( MonadAff SlamDataEffects m
    , MonadFork Exn.Error m
    , Ord k
    )
  ⇒ Int
  → k
  → (AVar Unit → { avar ∷ AVar Unit | r })
  → Cache.Cache k { avar ∷ AVar Unit | r }
  → m Unit
  → m Unit
  → m Unit
debounce ms key make cache init run = do
  avar ← laterVar ms $ void $ run *> Cache.remove key cache
  Cache.alter key (alterFn (make avar)) cache
  where
    alterFn a b = do
      case b of
        Just { avar } → liftAff $ killVar avar (Exn.error "debounce")
        Nothing → void $ fork init
      pure (Just a)

liftWithErr ∷ ∀ m a. Monad m ⇒ String → m (Maybe a) → ExceptT QE.QError m a
liftWithErr err =
  maybe (QE.throw err) pure <=< lift

updateCellModel ∷ CM.AnyCardModel → Card.Cell → Card.Cell
updateCellModel model cell = cell
  { value = cell.value
      { model = cell.value.model
          { model = model
          }
      }
  }
