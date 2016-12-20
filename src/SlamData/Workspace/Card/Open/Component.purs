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
  , module SlamData.Workspace.Card.Open.Component.State
  ) where

import SlamData.Prelude

import Data.Array as A
import Data.Lens ((.~), (?~), view)
import Data.List as L
import Data.Path.Pathy as Path
import Data.Unfoldable (unfoldr)

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.FileSystem.Resource as R
import SlamData.GlobalError as GE
import SlamData.Monad (Slam)
import SlamData.Notification as N
import SlamData.Quasar.Error as QE
import SlamData.Quasar.FS as Quasar
import SlamData.Render.Common (glyph)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Common.Render (renderLowLOD)
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Open.Component.Query (QueryP)
import SlamData.Workspace.Card.Open.Component.State (State, StateP, _levelOfDetails, _selected, _loading, initialState)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.MillerColumns.BasicItem.Component as MCI
import SlamData.Workspace.MillerColumns.Column.BasicFilter as MCF
import SlamData.Workspace.MillerColumns.Column.Component as MCC
import SlamData.Workspace.MillerColumns.Component as MC

import Utils.Path (AnyPath)

type DSL = H.ParentDSL State (MCI.BasicColumnsState R.Resource AnyPath) CC.CardEvalQuery (MCI.BasicColumnsQuery R.Resource AnyPath) Slam Unit
type HTML = H.ParentHTML (MCI.BasicColumnsState R.Resource AnyPath) CC.CardEvalQuery (MCI.BasicColumnsQuery R.Resource AnyPath) Slam Unit

openComponent ∷ CC.CardOptions → H.Component CC.CardStateP CC.CardQueryP Slam
openComponent options =
  CC.makeCardComponent
    { options
    , cardType: CT.Open
    , component: H.parentComponent
        { render
        , eval
        , peek: Just (peek ∘ H.runChildF)
        }
    , initialState: H.parentState initialState
    , _State: CC._OpenState
    , _Query: CC.makeQueryPrism CC._OpenQuery
    }

render ∷ State → HTML
render state =
  HH.div_
    [ renderHighLOD state
    , renderLowLOD (CT.cardIconLightImg CT.Open) id state.levelOfDetails
    ]

renderHighLOD ∷ State → HTML
renderHighLOD state =
  HH.div
    [ HP.classes
        $ (guard (state.loading) $> HH.className "loading")
        <> (guard (state.levelOfDetails ≠ High) $> B.hidden)
    ]
    [ HH.slot unit \_ →
        { component: MC.component itemSpec
        , initialState: H.parentState MC.initialState
        }
    ]

renderItem ∷ R.Resource → MC.ItemHTML
renderItem r =
  HH.div
    [ HP.classes
        [ HH.className "sd-miller-column-item-inner"
        , either
            (const $ HH.className "sd-miller-column-item-node")
            (const $ HH.className "sd-miller-column-item-leaf")
            (R.getPath r)
        ]
    ]
    [ HH.span_
        [ glyphForResource r
        , HH.text (R.resourceName r)
        ]
    ]

glyphForResource ∷ R.Resource → MC.ItemHTML
glyphForResource = case _ of
  R.File _ → glyph B.glyphiconFile
  R.Workspace _ → glyph B.glyphiconBook
  R.Directory _ → glyph B.glyphiconFolderOpen
  R.Mount (R.Database _) → glyph B.glyphiconHdd
  R.Mount (R.View _) → glyph B.glyphiconFile

eval ∷ CC.CardEvalQuery ~> DSL
eval = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k → do
    mbRes ← H.gets _.selected
    pure $ k $ Card.Open (fromMaybe R.root mbRes)
  CC.Load (Card.Open res) next → do
    let selectedResources = toResourceList res
    void $ H.query unit $ left $ H.action $ MC.Populate $ R.getPath <$> selectedResources
    for_ selectedResources \sel → do
      for_ (getColPath sel) \colPath →
        H.query unit $ right $
          H.ChildF colPath $ left $ H.action $ MCC.SetSelection (Just sel)
    H.modify (_selected ?~ res)
    pure next
  CC.Load _ next →
    pure next
  CC.ReceiveInput _ next →
    pure next
  CC.ReceiveOutput _ next →
    pure next
  CC.ReceiveState _ next →
    pure next
  CC.ReceiveDimensions dims next → do
    H.modify $
      _levelOfDetails .~
        if dims.width < 250.0 ∨ dims.height < 50.0
        then Low
        else High
    pure next
  CC.ModelUpdated _ next →
    pure next
  CC.ZoomIn next →
    pure next

peek ∷ ∀ x. MCI.BasicColumnsQuery R.Resource AnyPath x → DSL Unit
peek = coproduct (peekColumns) (const (pure unit))
  where
  peekColumns ∷ MC.Query R.Resource AnyPath x → DSL Unit
  peekColumns = case _ of
    MC.RaiseSelected _ selected _ → do
      H.modify (_selected .~ selected)
      CC.raiseUpdatedP CC.EvalModelUpdate
    _ → pure unit

itemSpec ∷ MCI.BasicColumnOptions R.Resource AnyPath
itemSpec =
  { render: MCI.component
      { label: R.resourceName
      , render: renderItem
      }
  , label: R.resourceName
  , load
  , isLeaf: maybe true isRight ∘ L.head
  , id: R.getPath
  }

load
  ∷ ∀ r
  . { path ∷ L.List AnyPath, filter ∷ String | r }
  → Slam { items ∷ L.List R.Resource, nextOffset ∷ Maybe Int }
load { path, filter } =
  case L.head path of
    Just (Left p) →
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
toResourceList ∷ R.Resource → L.List R.Resource
toResourceList res =
  (unfoldr \r → Tuple r <$> go r) res `L.snoc` R.Directory Path.rootDir
  where
  go ∷ R.Resource → Maybe R.Resource
  go = map (R.Directory ∘ fst) ∘ either Path.peel Path.peel ∘ R.getPath

getColPath ∷ R.Resource → Maybe (L.List AnyPath)
getColPath = maybe Nothing (Just ∘ toPathList ∘ Left ∘ fst) ∘ either Path.peel Path.peel ∘ R.getPath

toPathList ∷ AnyPath → L.List AnyPath
toPathList res =
  (unfoldr \r → Tuple r <$> either go go r) res `L.snoc` Left Path.rootDir
  where
  go ∷ ∀ b. Path.Path Path.Abs b Path.Sandboxed → Maybe AnyPath
  go = map (Left ∘ fst) ∘ Path.peel
