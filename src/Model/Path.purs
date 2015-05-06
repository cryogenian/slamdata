module Model.Path where

import Data.Array ()
import Data.DOM.Simple.Encode (encodeURIComponent, decodeURIComponent)
import Data.String (split, joinWith, trim, replace)
import qualified Data.String.Regex as Rgx

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
