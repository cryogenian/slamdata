module Model.Notebook.Port where

import Model.Resource
import Data.StrMap
import Data.Argonaut.Combinators
import qualified Data.Argonaut.Core as Ac
import qualified Data.Argonaut.Encode as Ae
import Optic.Core (prism', PrismP())
import Data.Maybe

type VarMapValue = String 

data Port
  = Closed
  | PortResource Resource
  | VarMap (StrMap VarMapValue)


_PortResource :: PrismP Port Resource
_PortResource = prism' PortResource $ \p -> case p of
  PortResource r -> Just r
  _ -> Nothing

_VarMap :: PrismP Port (StrMap VarMapValue)
_VarMap = prism' VarMap $ \p -> case p of
  VarMap m -> Just m
  _ -> Nothing
  



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
