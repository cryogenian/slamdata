-- | Common stuff to use in many components
module Model where

import Data.Maybe

data Sort = Asc | Desc

sortToString :: Sort -> String
sortToString Asc = "asc"
sortToString Desc = "desc"

sortFromString :: String -> Maybe Sort
sortFromString "asc" = Just Asc
sortFromString "desc" = Just Desc
sortFromString _ = Nothing
