module SlamData.Workspace.Card.InsertableCardType where

import Data.Array as Array
import Data.String as String
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

takesInput ∷ InsertableCardIOType -> InsertableCardType -> Boolean
takesInput io card = Foldable.elem card $ cardsThatTakeInput io

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

possibleToGetTo ∷ InsertableCardIOType -> InsertableCardType -> Boolean
possibleToGetTo io card =
  not $ Array.null $ Array.concat $ pathsBetweenIO io <$> inputsFor card

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

fromMaybePort ∷ Maybe Port → InsertableCardIOType
fromMaybePort input = maybe None fromPort input

printIOType :: InsertableCardIOType -> String
printIOType =
  case _ of
    Chart → "a chart"
    Data → "data"
    Download → "a download"
    Draftboard → "a draftboard"
    Markdown → "markdown"
    None → "to be the first card in a deck"
    Variables → "variables"

eitherOr :: Array String -> String
eitherOr strings =
  case Array.length strings of
    1 -> String.joinWith "" strings
    n ->
      "either "
        ++ String.joinWith ", " (Array.take (n - 1) strings)
        ++ " or "
        ++ String.joinWith "" (Array.drop (n - 1) strings)

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

print ∷ InsertableCardType -> String
print =
  CardType.cardName ∘ toCardType

aAn ∷ String -> String
aAn s =
  if String.toLower (String.take 1 s) `Foldable.elem` vowels
    then "An"
    else "A"
  where
  vowels = [ "a", "e", "i", "o", "u" ]

reason :: InsertableCardIOType -> InsertableCardType -> String
reason io card =
  aAn (print card) ++ " " ++ show (print card) ++ " card can't " ++ actual
    ++ " because it needs " ++ expected ++ action ++ "."
  where
  actual =
    case io of
      None -> "be the first card in a deck"
      _ -> "follow a card which outputs " ++ printIOType io
  expected =
    eitherOr $ printIOType <$> inputsFor card
  action =
    maybe "" (" to " ++ _) (printAction card)

printAction ∷ InsertableCardType -> Maybe String
printAction =
  case _ of
    CacheCard → Just "cache"
    DraftboardCard → Nothing
    OpenCard → Nothing
    QueryCard → Nothing
    SearchCard → Just "search"
    SetupChartCard → Just "set up a chart for"
    SetupDownloadCard → Just "setup a download for"
    SetupMarkdownCard → Nothing
    SetupVariablesCard → Nothing
    ShowChartCard → Just "show"
    ShowDownloadCard → Just "show"
    ShowMarkdownCard → Just "show"
    TableCard → Just "tabulate"
    TroubleshootCard → Just "troubleshoot"

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
