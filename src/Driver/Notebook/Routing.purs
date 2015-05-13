module Driver.Notebook.Routing where

import Config (notebookExtension)
import Control.Alt ((<|>))
import Control.Apply ((*>))
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.List (List())
import Data.Path.Pathy ((</>), rootDir, dir, file)
import Data.String (indexOf, length)
import Data.String.Regex (noFlags, regex, test, Regex())
import Data.Tuple (Tuple(..))
import Model.Action (Action(..), string2action)
import Model.Notebook.Cell (CellId(), string2cellId)
import Model.Resource (Resource(), newNotebook, _notebookPath)
import Optic.Core ((.~))
import Routing.Match (Match(), list, eitherMatch)
import Routing.Match.Class (lit, str)


data Routes
  = CellRoute Resource CellId Action
  | ExploreRoute Resource String
  | NotebookRoute Resource Action

routing :: Match Routes
routing = CellRoute <$> notebook <*> (lit "cells" *> cellId) <*> action
      <|> ExploreRoute <$> notebook <*> (lit "explore" *> str)
      <|> NotebookRoute <$> notebook <*> action
  where

  partsAndName :: Match (Tuple (List String) String)
  partsAndName = Tuple <$> (oneSlash *> (list notName)) <*> name

  notebookFromParts :: Tuple (List String) String -> Resource
  notebookFromParts (Tuple ps name) =
    newNotebook # _notebookPath .~ foldl (</>) rootDir (dir <$> ps) </> dir name

  notebook :: Match Resource
  notebook = notebookFromParts <$> partsAndName

  oneSlash :: Match Unit
  oneSlash = lit ""

  name :: Match String
  name = eitherMatch (notebookName <$> str)

  notName :: Match String
  notName = eitherMatch (pathPart <$> str)

  action :: Match Action
  action = (eitherMatch (string2action <$> str)) <|> pure View

  cellId :: Match CellId
  cellId = eitherMatch (string2cellId <$> str)


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

