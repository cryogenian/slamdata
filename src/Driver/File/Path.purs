module Driver.File.Path
  ( extractDir
  , updateSort
  , updateQ
  , updateSalt
  , setSort
  , updatePath
  , renderPath
  ) where

import Data.Array (reverse, filter, take, (!!), drop, intersect, length)
import Data.DOM.Simple.Encode (encodeURIComponent, decodeURIComponent)
import Data.Either (either)
import Data.Maybe
import Data.Path.Pathy
import Driver.File.Search (searchPath)
import Model.Resource
import Model.Sort (Sort(..), sort2string)
import qualified Data.String as Str
import qualified Data.String.Regex as Rgx
import Text.SlamSearch (mkQuery)
import Text.SlamSearch.Parser.Tokens (keyChars)
import Utils (endsWith, trimQuotes)

sortRgx :: Rgx.Regex
sortRgx = Rgx.regex "(sort=)([^/&]*)" Rgx.noFlags

qRgx :: Rgx.Regex
qRgx = Rgx.regex "(q=)([^/&]*)" Rgx.noFlags

saltRgx :: Rgx.Regex
saltRgx = Rgx.regex "(salt=)([^/&]*)" Rgx.noFlags

updateSort :: Sort -> String -> String
updateSort sort =
  Rgx.replace sortRgx res
  where res = "$1" <> sort2string sort

updateQ :: String -> String -> String
updateQ q =
  Rgx.replace qRgx res
  where res = "$1" <> encodeURIComponent q

updateSalt :: String -> String -> String
updateSalt salt old =
  if attempt /= old then attempt
  else updateSalt (salt <> salt) old
  where attempt = Rgx.replace saltRgx res old
        res = "$1" <> salt

setSort :: Sort -> String
setSort sort = "?sort=" <> sort2string sort <> "&q=&salt="

extractDir :: String -> DirPath
extractDir hash =
  maybe rootDir (rootDir </>) (parseAbsDir (getPath' hash) >>= sandbox rootDir)

getPath' :: String -> String
getPath' hash =
  fromMaybe "" do
    matches <- Rgx.match qRgx hash
    q <- decodeURIComponent <$> matches !! 2
    query <- either (const Nothing) Just (mkQuery q)
    searchPath query

updatePath :: AnyPath -> String -> String
updatePath path hash =
  let pathOld = "path:" <> getPath' hash
      pathNew = "path:" <> renderPath path
      replaced = Str.replace pathOld pathNew hash
  in if replaced == hash then
       updateQ pathNew hash
     else replaced

renderPath :: AnyPath -> String
renderPath ap =
  if 0 == (length $ intersect (Str.split "" rendered) (" ":keyChars))
  then rendered
  else "\"" <> rendered <> "\""
  where
  rendered = either renderFile renderDir ap
  renderDir path =
    printPath $ canonicalize $ maybe rootDir (rootDir </>) (sandbox rootDir path)

  renderFile path =
    printPath $ canonicalize $
    maybe (rootDir </> file "") (rootDir </>) (sandbox rootDir path)
