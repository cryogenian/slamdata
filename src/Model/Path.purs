module Model.Path where

import Data.Array ()
import Data.DOM.Simple.Encode (encodeURIComponent, decodeURIComponent)
import Data.Either (Either(..))
import Data.Path.Pathy
import Data.String (split, joinWith, trim, replace, drop, take, lastIndexOf, length)

import qualified Data.String.Regex as Rgx

type FilePath = AbsFile Sandboxed
type DirPath = AbsDir Sandboxed
type AnyPath = Either FilePath DirPath

infixl 6 <./>
(<./>) :: forall a s. Path a Dir s -> String -> Path a Dir s
(<./>) p ext = renameDir (changeDirExt $ const ext) p

changeDirExt :: (String -> String) -> DirName -> DirName
changeDirExt f (DirName name) =
  DirName ((if ext == "" then name else n) <> "." <> f ext)
  where
  idx = lastIndexOf "." name
  n = case idx of
    -1 -> name
    _ -> take idx name
  ext = case idx of
    -1 -> ""
    _ -> drop (idx + 1) name

dropDirExt :: DirName -> DirName
dropDirExt (DirName d) =
  DirName $ case idx of
    -1 -> d
    _ -> take idx d
  where
  idx = lastIndexOf "." d

dropNotebookExt :: String -> String
dropNotebookExt name = take (length name - length Config.notebookExtension - 1) name

decodeURIPath :: String -> String
decodeURIPath uri =
  decodeURIComponent $
  Rgx.replace (Rgx.regex "\\+" Rgx.noFlags{global=true}) " " uri

encodeURIPath :: String -> String
encodeURIPath path =
  joinWith "/" $
  joinWith "+" <$>
  (encodeURIComponent <$>) <$>
  split " " <$>
  split "/" path

hidePath :: String -> String -> String
hidePath path input =
  trim $
  replace ("+path:\"" <> path <> "\"") "" $
  replace ("+path:" <> path) "" input

cleanPath :: String -> String
cleanPath input =
  let rgx = Rgx.regex "\"" Rgx.noFlags{global=true}
      doubleSlash = Rgx.regex "//" Rgx.noFlags{global=true}
  in Rgx.replace doubleSlash "/" $
     "/" <> (trim $ Rgx.replace rgx "" input)
