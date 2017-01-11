module Test.SlamData.Property.Workspace.Deck.Dialog.Share.Model
  ( check
  ) where

import SlamData.Prelude

--import Control.Monad.Reader.Trans (runReaderT, ReaderT)
--import Control.Monad.Trampoline (Trampoline)

--import Test.StrongCheck ((<?>))
import Test.StrongCheck as SC
import Test.StrongCheck.Gen as Gen
import Test.StrongCheck.Arbitrary (arbitrary)

import Test.Property.Utils.Path (runArbFilePath, runArbDirPath)

import SlamData.Workspace.Deck.DeckId (DeckId)
import SlamData.Workspace.Deck.Dialog.Share.Model as M
import SlamData.Workspace.Deck.Dialog.Unshare.Component as UC

import Data.Array as Arr
import Data.List as List
import Utils.Path (DirPath, FilePath)
import Quasar.Advanced.Types as QT
import Data.Path.Pathy as Pt

import Utils.Trace (addTime)

type TestConfig =
  { workspaceCount ∷ Int
  , grantedToCount ∷ Int
  , deckCount ∷ Int
  , sourcesCount ∷ Int
  , cachesCount ∷ Int
  }

genUserId ∷ Gen.Gen QT.UserId
genUserId =  do
  name ← arbitrary
  domain ← arbitrary
  pure $ QT.UserId $ name ⊕ "@" ⊕ domain

genTokenId ∷ Gen.Gen QT.TokenId
genTokenId =  do
  token ← arbitrary
  pure $ QT.TokenId token

genGroupPath ∷ Gen.Gen QT.GroupPath
genGroupPath =  do
  group ← map runArbFilePath arbitrary
  pure $ Right group

genGrantedTo ∷ Gen.Gen QT.GrantedTo
genGrantedTo = do
  ix ← Gen.chooseInt 0 2
  case ix of
    0 → map QT.UserGranted genUserId
    1 → map QT.TokenGranted genTokenId
    _ → map QT.GroupGranted genGroupPath

genPermissionId ∷ Gen.Gen QT.PermissionId
genPermissionId = map QT.PermissionId arbitrary

genShareResume ∷ Gen.Gen M.ShareResume
genShareResume =  do
  view ← arbitrary
  pure
    if view
    then M.View
    else M.Edit

type Deck =
  { deckId ∷ DeckId
  , sources ∷ Array FilePath
  , caches ∷ Array FilePath
  }

type Workspace =
  { workspacePath ∷ DirPath
  , decks ∷ Array Deck
  }

genDeck ∷ Gen.Gen Deck
genDeck = do
  deckId ←  arbitrary
  sources ← map (map runArbFilePath) $ Gen.vectorOf conf.sourcesCount arbitrary
  caches ← map (map runArbFilePath) $ Gen.vectorOf conf.cachesCount arbitrary
  pure { deckId
       , caches
       , sources
       }

genWorkspace ∷ Gen.Gen Workspace
genWorkspace = do
  workspacePath ← map runArbDirPath arbitrary
  decks ← Gen.vectorOf conf.deckCount genDeck
  pure { workspacePath
       , decks
       }

type Shared =
  { permissions ∷ Array QT.PermissionR
  , sharedDeck ∷ Deck
  , grantedTo ∷ QT.GrantedTo
  , shareResume ∷ M.ShareResume
  , workspacePath ∷ DirPath
  }

genShared ∷ Gen.Gen Shared
genShared = do
  (workspaces ∷ Array Workspace) ← Gen.vectorOf conf.workspaceCount genWorkspace
  (grantedToS ∷ Array QT.GrantedTo) ← Gen.vectorOf conf.grantedToCount genGrantedTo
  traceAnyA "workspacePaths: "
  traceAnyA $ foldl add 0 $ map (Pt.depth ∘ _.workspacePath) workspaces
  arrArr ←
    for workspaces \workspace →  do
      let
        workspacePath = workspace.workspacePath
      for workspace.decks \deck → do
        shareResume ← genShareResume
        let
          sharingInput =
            { workspacePath: workspace.workspacePath
            , deckId: deck.deckId
            , sources: List.fromFoldable deck.sources
            , caches: List.fromFoldable deck.caches
            }
          actions = M.sharingActions sharingInput shareResume
        grantedToIx ← Gen.chooseInt 0 $ conf.grantedToCount - 1
        let
          grantedTo = unsafePartial $ fromJust $ grantedToS Arr.!! grantedToIx
        for actions \action → do
          id ← genPermissionId
          pure { action
               , grantedTo
               , id
               , deck
               , shareResume
               , workspacePath
               }
  let
    permissionsAndDecks = Arr.concat $ Arr.concat arrArr
    len = Arr.length permissionsAndDecks
    permissions =
      map (\{action, grantedTo, id} → {action, grantedTo, id})
        permissionsAndDecks

  ix ← Gen.chooseInt 0 $ len - 1

  let
    pad = unsafePartial $ fromJust $ permissionsAndDecks Arr.!! ix
    grantedTo = pad.grantedTo
    sharedDeck = pad.deck
    shareResume = pad.shareResume
    workspacePath = pad.workspacePath

  pure { sharedDeck
       , grantedTo
       , permissions
       , shareResume
       , workspacePath
       }




genAdjustedPermissions ∷ Gen.Gen UC.AdjustedPermissions
genAdjustedPermissions = do
  shared ← genShared
  let
    sharingInput =
      { workspacePath: shared.workspacePath
      , deckId: shared.sharedDeck.deckId
      , sources: List.fromFoldable shared.sharedDeck.sources
      , caches: List.fromFoldable shared.sharedDeck.caches
      }

    adjusted = addTime \_ → UC.adjustPermissions shared.permissions sharingInput
  traceAnyA "time"
  traceAnyA $ fst adjusted
  pure $ snd adjusted


newtype ArbAdjustedPermissions = ArbAdjustedPermissions UC.AdjustedPermissions

instance arbitraryArbAdjustedPermissions ∷ SC.Arbitrary ArbAdjustedPermissions where
  arbitrary = map ArbAdjustedPermissions genAdjustedPermissions

conf ∷ TestConfig
conf =
  { workspaceCount: 10
  , grantedToCount: 5
  , deckCount: 10
  , sourcesCount: 2
  , cachesCount: 2
  }

check ∷ forall eff. SC.SC eff Unit
check =
  SC.quickCheck' 20 \(a ∷ ArbAdjustedPermissions) → true
