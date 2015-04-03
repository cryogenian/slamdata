-- | Sort direction
module Model.Sort where

data Sort = Asc | Desc

-- | revese sort
notSort :: Sort -> Sort
notSort Asc = Desc
notSort _ = Asc

sort2string :: Sort -> String
sort2string Asc = "asc"
sort2string Desc = "desc"

instance eqSort :: Eq Sort where
  (==) Asc Asc = true
  (==) Desc Desc = true
  (==) _ _ = false
  (/=) a b = not $ a == b
