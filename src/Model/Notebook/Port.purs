module Model.Notebook.Port where

import Model.Resource
import Data.StrMap
import Data.Argonaut.Combinators
import qualified Data.Argonaut.Core as Ac
import qualified Data.Argonaut.Encode as Ae


type VarMapValue = String 

data Port
  = Closed
  | PortResource Resource
  | VarMap (StrMap VarMapValue)


instance portEncode :: Ae.EncodeJson Port where
  encodeJson Closed = Ac.jsonEmptyObject
  encodeJson (PortResource res) =
    "type" := "resource"
    ~> "path" := resourcePath res
    ~> Ac.jsonEmptyObject
  encodeJson (VarMap map) =
    "type" := "map"
    ~> "content" := map
    ~> Ac.jsonEmptyObject
