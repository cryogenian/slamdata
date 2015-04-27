module Model.Path where

import Data.List
import Utils (encodeURIComponent, decodeURIComponent)
import qualified Data.String as Str
import qualified Data.String.Regex as Rgx

data Path = Path (List String) String

emptyPath :: Path
emptyPath = Path Nil ""

getName :: Path -> String
getName (Path _ name) = name

parent :: Path -> Path
parent (Path dir name) =
  case reverse dir of
    Cons p t -> Path (reverse t) p
    Nil -> Path Nil ""


updateName :: String -> Path -> Path
updateName name (Path ps _) = Path ps name

path2str :: Path -> String
path2str (Path lst name) = Rgx.replace rgx "/" strpath
  where
  strpath = 
    "/" <> (Str.joinWith "/" $ toArray $  lst) <> "/" <> name
  rgx = Rgx.regex "(/+)" Rgx.noFlags{global = true}


decodeURIPath :: String -> String
decodeURIPath uri =
  decodeURIComponent $
  Rgx.replace (Rgx.regex "\\+" Rgx.noFlags{global=true}) " " uri

encodeURIPath :: String -> String
encodeURIPath path =
  Str.joinWith "/" $
  Str.joinWith "+" <$>
  (encodeURIComponent <$>) <$>
  Str.split " " <$>
  Str.split "/" path


hidePath :: String -> String -> String
hidePath path input =
  Str.trim $
  Str.replace ("+path:\"" <> path <> "\"") "" $
  Str.replace ("+path:" <> path) "" input

cleanPath :: String -> String
cleanPath input =
  let rgx = Rgx.regex "\"" Rgx.noFlags{global=true}
      doubleSlash = Rgx.regex "//" Rgx.noFlags{global=true}
  in Rgx.replace doubleSlash "/" $
     "/" <> (Str.trim $ Rgx.replace rgx "" input)
