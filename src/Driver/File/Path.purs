module Driver.File.Path (renderPath) where

import Data.Array (intersect, length)
import Data.Either (either)
import Data.Maybe (maybe)
import Data.Path.Pathy ((</>), printPath, canonicalize, sandbox, rootDir, file)
import Model.Path (AnyPath())
import Text.SlamSearch.Parser.Tokens (keyChars)

import qualified Data.String as Str

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
