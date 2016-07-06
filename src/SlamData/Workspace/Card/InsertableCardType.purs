module SlamData.Workspace.Card.InsertableCardType where

import Data.Array as Array
import Data.Foldable as Foldable
import SlamData.Prelude
import SlamData.Workspace.Card.CardType as CardType
import SlamData.Workspace.Card.CardType (CardType)
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Port (Port)
import Utils (singletonValue)

data InsertableCardType
  = CacheCard
  | DraftboardCard
  | OpenCard
  | QueryCard
  | SearchCard
  | SetupChartCard
  | SetupDownloadCard
  | SetupMarkdownCard
  | SetupVariablesCard
  | ShowChartCard
  | ShowDownloadCard
  | ShowMarkdownCard
  | TableCard
  | TroubleshootCard

data InsertableCardIOType
  = Chart
  | Data
  | Download
  | Draftboard
  | Markdown
  | None
  | Variables

derive instance eqInsertableCardType ∷ Eq InsertableCardType
derive instance eqInsertableCardIOType ∷ Eq InsertableCardIOType

inputs ∷ Array (InsertableCardType × (Array InsertableCardIOType))
inputs =
  [ CacheCard × [ Data ]
  , DraftboardCard × [ None ]
  , OpenCard × [ None ]
  , QueryCard × [ None, Data, Variables ]
  , SearchCard × [ Data ]
  , SetupChartCard × [ Data ]
  , SetupDownloadCard × [ Data ]
  , SetupMarkdownCard × [ None ]
  , SetupVariablesCard × [ None ]
  , ShowChartCard × [ Chart ]
  , ShowDownloadCard × [ Download ]
  , ShowMarkdownCard × [ Markdown ]
  , TableCard × [ Data ]
  , TroubleshootCard × [ Variables, Data ]
  ]

outputs ∷ Array (InsertableCardType × InsertableCardIOType)
outputs =
  [ CacheCard × Data
  , DraftboardCard × Draftboard
  , OpenCard × Data
  , QueryCard × Data
  , SearchCard × Data
  , SetupChartCard × Chart
  , SetupDownloadCard × Download
  , SetupMarkdownCard × Markdown
  , SetupVariablesCard × Variables
  , ShowChartCard × Chart
  , ShowDownloadCard × Download
  , ShowMarkdownCard × Variables
  , TableCard × Data
  , TroubleshootCard × Variables
  ]

contains ∷ ∀ a. (Eq a) ⇒ a → Array a → Boolean
contains x =
  isJust ∘ Array.elemIndex x

inputsFor ∷ InsertableCardType → Array InsertableCardIOType
inputsFor card =
  Array.concat $ map snd $ Array.filter (eq card ∘ fst) inputs

outputFor ∷ InsertableCardType → Maybe InsertableCardIOType
outputFor card =
  singletonValue Nothing (const Nothing) (map snd $ Array.filter (eq card ∘ fst) outputs)

cardsThatOutput ∷ InsertableCardIOType → Array InsertableCardType
cardsThatOutput io =
  map fst $ Array.filter (eq io ∘ snd) outputs

cardsThatTakeInput ∷ InsertableCardIOType → Array InsertableCardType
cardsThatTakeInput io =
  map fst $ Array.filter (contains io ∘ snd) inputs

type Path =
  { c ∷ Array InsertableCardType, io ∷ Array InsertableCardIOType, f ∷ Boolean }

purePath ∷ InsertableCardIOType → Path
purePath io =
  { c: [], io: [io], f: false }

cardPathsFromPaths ∷ Array Path → Array (Array InsertableCardType)
cardPathsFromPaths = map _.c

allPathsFinished ∷ Array Path → Boolean
allPathsFinished = Foldable.all _.f

cardPathsBetween ∷ Maybe InsertableCardType → InsertableCardType -> Array (Array InsertableCardType)
cardPathsBetween from = cardPathsFromPaths ∘ pathsBetween from

pathsBetween ∷ Maybe InsertableCardType → InsertableCardType → Array Path
pathsBetween fromCard toCard =
  Array.concat
    $ pathsBetweenIO
        <$> maybe [] Array.singleton (maybe (Just None) outputFor fromCard)
        <*> inputsFor toCard

cardPathNeeded ∷ Array (Array InsertableCardType) → Boolean
cardPathNeeded cardPaths =
  not $ Array.null cardPaths || [] `Foldable.elem` cardPaths

pathsBetweenIO ∷ InsertableCardIOType → InsertableCardIOType → Array Path
pathsBetweenIO fromIO toIO =
  go $ [ purePath toIO ]
  where
  go ∷ Array Path → Array Path
  go paths | allPathsFinished paths = paths
  go paths | otherwise =
    go $ Array.concat $ expandPath fromIO <$> paths

expandPath ∷ InsertableCardIOType → Path → Array Path
expandPath fromIO initialPath | initialPath.f =
  [ initialPath ]
expandPath fromIO initialPath | maybe true (eq fromIO) (Array.head initialPath.io) =
  [ initialPath { f = true } ]
expandPath fromIO initialPath | otherwise =
  maybe [] f $ Array.head initialPath.io
  where
  f ∷ InsertableCardIOType → Array Path
  f initialIO = Array.concat $ g <$> (cardsThatOutput initialIO)

  g ∷ InsertableCardType → Array Path
  g card = h card `Array.mapMaybe` (inputsFor card)

  h ∷ InsertableCardType → InsertableCardIOType → Maybe Path
  h card io | io `Foldable.elem` initialPath.io = Nothing
  h card io =
    Just { c: pathCards, io: pathIO, f: pathFinished }
    where
    pathFinished = fromIO `Foldable.elem` pathIO
    pathIO = Array.cons io initialPath.io
    pathCards = Array.cons card initialPath.c

fromPort ∷ Port → InsertableCardIOType
fromPort =
  case _ of
    Port.Chart _ → Chart
    Port.DownloadOptions _ → Download
    Port.Draftboard → Draftboard
    Port.SlamDown _ → Markdown
    Port.TaggedResource _ → Data
    Port.VarMap _ → Variables
    _ → None

toCardType ∷ InsertableCardType → CardType
toCardType =
  case _ of
    CacheCard → CardType.Cache
    DraftboardCard → CardType.Draftboard
    OpenCard → CardType.Open
    QueryCard → CardType.Ace CardType.SQLMode
    SearchCard → CardType.Search
    SetupChartCard → CardType.ChartOptions
    SetupDownloadCard → CardType.DownloadOptions
    SetupMarkdownCard → CardType.Ace CardType.MarkdownMode
    SetupVariablesCard → CardType.Variables
    ShowChartCard → CardType.Chart
    ShowDownloadCard → CardType.Download
    ShowMarkdownCard → CardType.Markdown
    TableCard → CardType.Table
    TroubleshootCard → CardType.Troubleshoot

fromCardType ∷ CardType → Maybe InsertableCardType
fromCardType =
  case _ of
    CardType.Cache → Just CacheCard
    CardType.Draftboard → Just DraftboardCard
    CardType.Open → Just OpenCard
    CardType.Ace CardType.SQLMode → Just QueryCard
    CardType.Search → Just SearchCard
    CardType.ChartOptions → Just SetupChartCard
    CardType.DownloadOptions → Just SetupDownloadCard
    CardType.Ace CardType.MarkdownMode → Just SetupMarkdownCard
    CardType.Variables → Just SetupVariablesCard
    CardType.Chart → Just ShowChartCard
    CardType.Download → Just ShowDownloadCard
    CardType.Markdown → Just ShowMarkdownCard
    CardType.Table → Just TableCard
    CardType.Troubleshoot → Just TroubleshootCard
    _ -> Nothing
