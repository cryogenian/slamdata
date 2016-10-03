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

module Utils.Ace where

import Prelude

import Ace.Types (Editor, EditSession, ACE, Position(..), Command(..), Range)
import Ace.Editor as Editor
import Ace.EditSession as Session
import Ace.Document as Document
import Ace.Anchor as Anchor
import Ace.Range as Range
import Ace.KeyBinding as KeyBinding
import Ace.Marker as Marker

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (newRef, writeRef, readRef, REF)
import Data.Argonaut (Json, (:=), (~>), (.?), decodeJson, jsonEmptyObject)
import Data.Array as Arr
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Traversable as T
import Data.Tuple (Tuple(..), fst, snd)
import DOM (DOM)

import Test.StrongCheck.Arbitrary as SCA
import Test.StrongCheck.Gen as Gen

type Effects e = (ace :: ACE, dom :: DOM, ref :: REF|e)

readonlyClazz :: String
readonlyClazz = "readonly-highlight"

readonlyTag :: String
readonlyTag = "readonly"

addMarker :: forall e. Range -> EditSession -> Eff (Effects e) Int
addMarker r session = Session.addMarker r readonlyClazz readonlyTag false session

type RangeRec =
  { startRow :: Int
  , startColumn :: Int
  , endRow :: Int
  , endColumn :: Int
  }

genRangeRec ∷ Gen.Gen RangeRec
genRangeRec = do
  startRow ← SCA.arbitrary
  startColumn ← SCA.arbitrary
  endRow ← SCA.arbitrary
  endColumn ← SCA.arbitrary
  pure { startRow, startColumn, endRow, endColumn }

eqRangeRec :: RangeRec -> RangeRec -> Boolean
eqRangeRec r rr =
  r.startRow == rr.startRow
  && r.startColumn == rr.startColumn
  && r.endRow == rr.endRow
  && r.endColumn == rr.endColumn

encodeRangeRec :: RangeRec -> Json
encodeRangeRec p
   = "startRow" := p.startRow
  ~> "startColumn" := p.startColumn
  ~> "endRow" := p.endRow
  ~> "endColumn" := p.endColumn
  ~> jsonEmptyObject

decodeRangeRec :: Json -> Either String RangeRec
decodeRangeRec = decodeJson >=> \obj ->
  { startRow: _, startColumn: _, endRow: _, endColumn: _ }
    <$> obj .? "startRow"
    <*> obj .? "startColumn"
    <*> obj .? "endRow"
    <*> obj .? "endColumn"


readOnly :: forall e. Editor -> RangeRec -> Eff (Effects e) Unit
readOnly editor {startRow, startColumn, endRow, endColumn} = do
  session <- Editor.getSession editor
  document <- Session.getDocument session
  startAnchor <- Document.createAnchor startRow startColumn document
  endAnchor <- Document.createAnchor endRow endColumn document
  Anchor.setInsertRight true endAnchor

  range <- Range.create startRow startColumn endRow endColumn
  markerRef <- addMarker range session >>= newRef
  let rerenderMarker _ = do
        readRef markerRef >>= flip Session.removeMarker session
        Position {row: startRow, column: startColumn}
          <- Anchor.getPosition startAnchor
        Position {row: endRow, column: endColumn}
          <- Anchor.getPosition endAnchor
        markRange <- Range.create startRow startColumn endRow endColumn
        newMid <- addMarker markRange session
        writeRef markerRef newMid
  Anchor.onChange startAnchor rerenderMarker
  Anchor.onChange endAnchor rerenderMarker

  Editor.getKeyBinding editor
    >>= KeyBinding.addKeyboardHandler \_ hs kstring kcode _ -> do
      if hs == -1 || (kcode <= 40 && kcode >= 37) -- arrows
        then pure Nothing
        else do
        Position {row: startRow, column: startColumn}
          <- Anchor.getPosition startAnchor
        Position {row: endRow, column: endColumn}
          <- Anchor.getPosition endAnchor
        selectedRange <- Editor.getSelectionRange editor
        newRange <-
          if kstring == "backspace"
          then Range.create startRow (startColumn + 1) endRow endColumn
          else if kstring == "delete" || (kstring == "d" && hs == 1)
               then Range.create startRow startColumn endRow (endColumn - 1)
               else Range.create startRow (startColumn + 1) endRow (endColumn - 1)
        intersected <- Range.intersects newRange selectedRange
        pure if intersected
             then Just {command: Null, passEvent: false}
             else Nothing
  Editor.navigateFileEnd editor


getRangeRecs :: forall e. Editor -> Eff (Effects e) (Array RangeRec)
getRangeRecs editor = do
  markers <- Editor.getSession editor >>= Session.getMarkers
  tpls <- T.traverse (\m -> Tuple m <$> Marker.getType m) markers
  ranges <- T.for (map fst $ Arr.filter (eq "readonly" <<< snd) tpls) Marker.getRange
  T.for (Arr.catMaybes ranges) mkRangeRec
  where
  mkRangeRec :: Range -> Eff (Effects e) RangeRec
  mkRangeRec r = do
    Position {row: startRow, column: startColumn} <- Range.getStart r
    Position {row: endRow, column: endColumn} <- Range.getEnd r
    pure { startRow, startColumn, endRow, endColumn }
