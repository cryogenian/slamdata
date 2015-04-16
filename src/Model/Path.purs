module Model.Path where

import Data.List
import Utils (encodeURIComponent, decodeURIComponent)
import qualified Data.String as Str
import qualified Data.String.Regex as Rgx

data Path = Path (List String) String

path2str :: Path -> String
path2str (Path lst name) =
  "/" <> (Str.joinWith "/" $ toArray lst) <> "/" <> name


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
