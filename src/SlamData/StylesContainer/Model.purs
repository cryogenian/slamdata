module SlamData.StylesContainer.Model where

import Prelude

import Data.Map as Map
import Data.Maybe as M
import Data.Either as E
import Data.String as S
import Data.URI as Uri
import Data.Array ((\\))
import Data.Array as Arr
import Data.Tuple as Tpl
import Control.Monad.Error.Class as Ec
import Control.UI.Browser (decodeURIComponent, encodeURIComponent)

newtype StyleURL = StyleURL String
runStyleURL :: StyleURL -> String
runStyleURL (StyleURL s) = s

instance eqStyleURL :: Eq StyleURL where
  eq (StyleURL s) (StyleURL ss) = s == ss

instance ordStyleURL :: Ord StyleURL where
  compare (StyleURL s) (StyleURL ss) = compare s ss


styleQueryParamName :: String
styleQueryParamName = "cssStyleSheets"

extractStyleURLs :: Map.Map String String -> Array StyleURL
extractStyleURLs m =
  Map.lookup styleQueryParamName m
  # M.fromMaybe ""
  # styleURIsFromStr
  where
  styleURIsFromStr :: String -> Array StyleURL
  styleURIsFromStr s =
    Arr.filter (/= (StyleURL ""))
    $ map (Tpl.snd >>> StyleURL)
    $ Arr.filter (E.isRight <<< Tpl.fst)
    $ map (\s -> Tpl.Tuple (Uri.runParseURIRef s) s)
    $ S.split ","
    $ decodeURIComponent s

printStyleURLs:: Array StyleURL -> String
printStyleURLs arr =
  encodeURIComponent
  $ S.joinWith ","
  ((runStyleURL <$> arr) \\ SlamData.Config.defaultStyleSheets)
