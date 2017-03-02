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

module SlamData.Workspace.Card.Open.Component
  ( openComponent
  , module SlamData.Workspace.Card.Open.Component.Query
  ) where

import SlamData.Prelude

import Data.Array as A
import Data.Lens (view)
import Data.List as L
import Data.Path.Pathy as Path
import Data.Unfoldable (unfoldr)

import Halogen as H
import Halogen.Component.Utils (busEventSource)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.FileSystem.Resource as R
import SlamData.GlobalError as GE
import SlamData.GlobalMenu.Bus as GMB
import SlamData.Monad (Slam)
import SlamData.Notification as N
import SlamData.Quasar.Error as QE
import SlamData.Quasar.FS as Quasar
import SlamData.Render.Common (glyph)
import SlamData.Wiring as Wiring
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Open.Component.Query (Query(..))
import SlamData.Workspace.Card.Open.Component.Query as Q
import SlamData.Workspace.Card.Open.Component.State (State, initialState)
import SlamData.Workspace.LevelOfDetails as LOD
import SlamData.Workspace.MillerColumns.BasicItem.Component as MCI
import SlamData.Workspace.MillerColumns.Column.BasicFilter as MCF
import SlamData.Workspace.MillerColumns.Component as MC

import Utils.Path (AnyPath)

type ColumnsQuery = MC.Query R.Resource AnyPath Void
type DSL = CC.InnerCardParentDSL State Query ColumnsQuery Unit
type HTML =  CC.InnerCardParentHTML Query ColumnsQuery Unit

openComponent ∷ CC.CardOptions → CC.CardComponent
openComponent =
  CC.makeCardComponent CT.Open $ H.lifecycleParentComponent
    { render: render
    , eval: evalCard ⨁ evalOpen
    , initialState: const initialState
    , receiver: const Nothing
    , initializer: Just $ right $ H.action Init
    , finalizer: Nothing
    }

render ∷ State → HTML
render state =
  HH.slot unit (MC.component itemSpec) (pathToColumnData (Left Path.rootDir)) handleMessage

handleMessage ∷ MC.Message' R.Resource AnyPath Void → Maybe (CC.InnerCardQuery Query Unit)
handleMessage =
  either
    (\(MC.SelectionChanged _ sel) → Just $ H.action $ right ∘ UpdateSelection sel)
    absurd

renderItem ∷ R.Resource → MCI.BasicItemHTML
renderItem r =
  HH.div
    [ HP.classes
        [ HH.ClassName "sd-miller-column-item-inner"
        , either
            (const $ HH.ClassName "sd-miller-column-item-node")
            (const $ HH.ClassName "sd-miller-column-item-leaf")
            (R.getPath r)
        ]
    ]
    [ HH.span_
        [ glyphForResource r
        , HH.text (R.resourceName r)
        ]
    ]

glyphForResource ∷ R.Resource → MCI.BasicItemHTML
glyphForResource = case _ of
  R.File _ → glyph B.glyphiconFile
  R.Workspace _ → glyph B.glyphiconBook
  R.Directory _ → glyph B.glyphiconFolderOpen
  R.Mount (R.Database _) → glyph B.glyphiconHdd
  R.Mount (R.View _) → glyph B.glyphiconFile

evalOpen ∷ Query ~> DSL
evalOpen = case _ of
  Q.Init next → do
    { auth } ← Wiring.expose
    H.subscribe
      $ busEventSource (\msg -> right (Q.HandleSignInMessage msg H.Listening)) auth.signIn
    pure next
  Q.HandleSignInMessage message next → do
    when (message ≡ GMB.SignInSuccess) do
      void $ H.query unit $ H.action $ MC.Reload
    pure next
  Q.UpdateSelection selected next → do
    H.put selected
    H.raise CC.modelUpdate
    pure next

evalCard ∷ CC.CardEvalQuery ~> DSL
evalCard = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k → do
    mbRes ← H.get
    pure $ k $ Card.Open (fromMaybe R.root mbRes)
  CC.Load (Card.Open res) next → do
    void $ H.query unit $ H.action $ MC.Populate $ pathToColumnData $ R.getPath res
    H.put (Just res)
    pure next
  CC.Load _ next →
    pure next
  CC.ReceiveInput _ _ next →
    pure next
  CC.ReceiveOutput _ _ next →
    pure next
  CC.ReceiveState _ next →
    pure next
  CC.ReceiveDimensions dims reply → do
    pure $ reply
      if dims.width < 250.0 ∨ dims.height < 50.0
      then LOD.Low
      else LOD.High

itemSpec ∷ MCI.BasicColumnOptions R.Resource AnyPath
itemSpec =
  { render: MCI.component
      { label: R.resourceName
      , render: renderItem
      }
  , label: R.resourceName
  , load
  , isLeaf: isRight
  , id: R.getPath
  }

load
  ∷ ∀ r
  . { path ∷ AnyPath, filter ∷ String | r }
  → Slam { items ∷ L.List R.Resource, nextOffset ∷ Maybe Int }
load { path, filter } =
  case path of
    Left p →
      Quasar.children p >>= case _ of
        Left err → handleError err $> noResult
        Right rs → do
          let filenameFilter = MCF.mkFilter filter
          pure
            { items: L.fromFoldable (A.filter (filenameFilter ∘ view R._name) rs)
            , nextOffset: Nothing
            }
    _ → pure noResult
  where
  noResult = { items: L.Nil, nextOffset: Nothing }

handleError ∷ QE.QError → Slam Unit
handleError err =
  case GE.fromQError err of
    Left msg →
      N.error
        "There was a problem fetching the directory listing"
        (Just $ N.Details msg)
        Nothing
        Nothing
    Right ge →
      GE.raiseGlobalError ge

-- | For a filesystem path, construct a list of all of the sub-paths up to the
-- | current point. This is required as column-view paths (in contrast to
-- | filesystem paths) are required to be in an unfolded form. It's cheating a
-- | little bit, as some of the intermediate steps may be mounts, etc. but we
-- | don't know that so always produce directories, but for the usage here that
-- | doesn't matter.
pathToColumnData ∷ AnyPath → AnyPath × L.List R.Resource
pathToColumnData path =
  case L.unsnoc (toPathList path) of
    Just { init, last } →
      last × map (either R.Directory R.File) init
    Nothing →
      Left Path.rootDir × L.Nil

toPathList ∷ AnyPath → L.List AnyPath
toPathList res =
  (unfoldr \r → Tuple r <$> either go go r) res `L.snoc` Left Path.rootDir
  where
  go ∷ ∀ b. Path.Path Path.Abs b Path.Sandboxed → Maybe AnyPath
  go = map (Left ∘ fst) ∘ Path.peel
