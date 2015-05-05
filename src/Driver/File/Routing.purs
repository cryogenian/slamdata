module Driver.File.Routing (routing, Routes(..)) where

import Data.Either
import Control.Alt ((<|>))
import Routing.Match (Match(), eitherMatch)
import Routing.Match.Class (param)
import Text.SlamSearch (mkQuery)
import Text.SlamSearch.Types (SearchQuery())
import qualified Model.Sort as M

data Routes
  = Salted M.Sort SearchQuery String
  | SortAndQ M.Sort SearchQuery
  | Sort M.Sort
  | Index

getSalt :: String -> Either String String
getSalt input =
  if input /= "" then Right input
  else Left "incorrect salt"

routing :: Match Routes
routing = salted <|> bothRoute <|> oneRoute <|> index
  where
  salted = Salted <$> sort <*> query <*> salt
  bothRoute = SortAndQ <$> sort <*> query
  oneRoute = Sort <$> sort
  index = pure Index
  sort = eitherMatch (M.string2sort <$> param "sort")
  query = eitherMatch (mkQuery <$> param "q")
  salt = eitherMatch (getSalt <$> param "salt")
  
