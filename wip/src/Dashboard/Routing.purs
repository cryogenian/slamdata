module Dashboard.Routing
       ( routing
       , routeSignal
       , Routes(..)
       ) where
import Prelude

import Config (notebookExtension)
import Control.Alt ((<|>))
import Control.Apply ((*>))
import Control.Monad.Aff (Aff())
import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(..))
import Model.Action (Action(..), string2action)
import Model.Resource (Resource(..))
import Routing.Match (Match(), list, eitherMatch)
import Routing.Match.Class (lit, str)
import Data.Foldable (foldl)
import Data.List (List(..))
import Data.Path.Pathy ((</>), rootDir, dir, file)
import Data.Tuple (Tuple(..))
import Data.String.Regex (noFlags, regex, test, Regex())
import Data.Either (Either(..))
import Data.Lens ((.~))
import Data.Functor.Coproduct (left)
import Notebook.Cell.CellId (CellId(), string2cellId)
import Halogen.Driver (Driver())
import Notebook.Component (NotebookQuery(..), initialNotebook, _browserFeatures)
import Notebook.Effects (NotebookRawEffects(), NotebookEffects())
import Dashboard.Component (QueryP(), toDashboard, toNotebook)
import Utils.Path (decodeURIPath)
import Debug.Trace (traceAnyA)
import Halogen (action, liftEff', liftH)
import Routing (matchesAff')
import DOM.BrowserFeatures.Detectors (detectBrowserFeatures)

data Routes
  = CellRoute Resource CellId Action
  | ExploreRoute Resource
  | NotebookRoute Resource Action

routing :: Match Routes
routing
  =   ExploreRoute <$> (oneSlash *> lit "explore" *> explored)
  <|> CellRoute <$> notebook <*> (lit "cells" *> cellId) <*> action
  <|> NotebookRoute <$> notebook <*> action

  where
  oneSlash :: Match Unit
  oneSlash = lit ""

  explored :: Match Resource
  explored = map fileFromParts fileParts

  fileFromParts :: Tuple (List String) String -> Resource
  fileFromParts (Tuple ps nm) =
    File $ foldl (</>) rootDir (map dir ps) </> file nm

  fileParts :: Match (Tuple (List String) String)
  fileParts = Tuple <$> (oneSlash *> list str) <*> str

  notebook :: Match Resource
  notebook = notebookFromParts <$> partsAndName

  notebookFromParts :: Tuple (List String) String -> Resource
  notebookFromParts (Tuple ps nm) =
    Notebook $ foldl (</>) rootDir (map dir ps) </> dir nm

  partsAndName :: Match (Tuple (List String) String)
  partsAndName = Tuple <$> (oneSlash *> (list notName)) <*> name

  name :: Match String
  name = eitherMatch $ map notebookName str

  notName :: Match String
  notName = eitherMatch $ map pathPart str

  notebookName :: String -> Either String String
  notebookName input | checkExtension input = Right input
                     | otherwise = Left input

  pathPart :: String -> Either String String
  pathPart input | input == "" || checkExtension input = Left "incorrect path part"
                 | otherwise = Right input

  extensionRegex :: Regex
  extensionRegex = regex ("\\." <> notebookExtension <> "$") noFlags

  checkExtension :: String -> Boolean
  checkExtension = test extensionRegex

  action :: Match Action
  action = (eitherMatch $ map string2action str) <|> pure View

  cellId :: Match CellId
  cellId = eitherMatch $ map string2cellId str

routeSignal :: Driver QueryP NotebookRawEffects -> Aff NotebookEffects Unit
routeSignal driver = do
  Tuple oldRoute newRoute <- matchesAff' decodeURIPath routing
  case newRoute of
    CellRoute res cellId editable -> notebook res editable $ Just cellId
    NotebookRoute res editable -> notebook res editable Nothing
    ExploreRoute res -> pure unit
  where
  notebook :: Resource -> Action -> Maybe CellId -> Aff NotebookEffects Unit
  notebook res editable viewing = do
    browserFeatures <- liftEff detectBrowserFeatures
    driver $ toNotebook
      $ SetState (initialNotebook # _browserFeatures .~ browserFeatures)
