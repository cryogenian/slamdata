module Model.Notebook.Search (queryToSQL, needFields) where

import Control.Apply (lift2)
import Data.Either 
import Data.Maybe
import Data.Date (fromString)
import Data.Semiring.Free
import Data.Foldable
import Data.String (joinWith)
import Data.String.Regex (regex, noFlags, replace, Regex())
import Text.SlamSearch (mkQuery, check)
import Text.SlamSearch.Types
import Global


queryToSQL :: [String] -> SearchQuery -> String
queryToSQL fields query =
  "select * from {{path}} where " <> 
  (joinWith " OR " $
   (\x -> "(" <> x <> ")") <$> 
   joinWith " AND " <$> 
   (runFree $ (termToSQL fields) <$> query))

  

needFields :: SearchQuery -> Boolean
needFields query =
  check unit query needFields'
  where
  needFields' :: Unit -> Term -> Boolean
  needFields' _ (Term {labels: []}) = true
  needFields' _ _ = false

termToSQL :: [String] -> Term -> String 
termToSQL fields (Term {include: include, predicate: p, labels: ls}) =
  if not include 
  then "NOT (" <> (termToSQL fields $ Term {include: true, predicate: p, labels: ls}) <> ")"
  else renderPredicate p $ labelsProjection fields ls


renderPredicate :: Predicate -> [String] -> String
renderPredicate p prj =
  joinWith " OR " (predicateToSQL p <$> prj)


predicateToSQL :: Predicate -> String -> String
predicateToSQL (Contains (Range v v')) s = range v v' s 
predicateToSQL (Contains (Text v)) s =
  joinWith " OR " $
  [s <> " LIKE '%" <> v <> "%'",
   s <> " = '" <> v <> "'"] <>
  (if needUnq v then [ s <> " = " <> v ] else []) <> 
  (if needDate v then [ s <> " = " <> date ] else []) 
  where
  quoted = "'" <> v <> "'"
  date = "DATE " <> quoted 
predicateToSQL (Contains (Tag v)) s = predicateToSQL (Contains (Text v)) (s <> "[*]")
predicateToSQL (Eq v) s = qUnQ s "=" v
predicateToSQL (Gt v) s = qUnQ s ">" v
predicateToSQL (Gte v) s = qUnQ s ">=" v 
predicateToSQL (Lt v) s = qUnQ s "<" v 
predicateToSQL (Lte v) s = qUnQ s "<=" v 
predicateToSQL (Ne v) s = qUnQ s "<>" v
predicateToSQL (Like v) s = s <> " LIKE " <>  glob2like v
predicateToSQL _ _ = ""

range :: String -> String -> String -> String
range v v' s =
  joinWith " OR " $
  [ forR quoted quoted' ] <> 
  (if needUnq v && needUnq v' then [ forR v v' ] else [ ]) <>
  (if needDate v && needDate v' then [ forR date date' ] else [ ])
  where
  quoted = "'" <> v <> "'"
  quoted' = "'" <> v' <> "'"

  date = "DATE " <> quoted
  date' = "DATE " <> quoted'

  forR :: String -> String -> String
  forR v v' =
    fold ["(", s, " >= ", v, " AND ", s, " <= ", v', ")"]

qUnQ :: String -> String -> Value -> String
qUnQ s op v = (\x -> "(" <> x <>  ")") $ 
  joinWith " OR " $ 
  [ forV quoted ] <>
  (if needUnq unquoted then [ forV unquoted ] else [] ) <>
  (if needDate unquoted then [ forV date ] else [] )

  where
  unquoted = valueToSQL v 
  quoted = "'" <> unquoted <> "'"
  date = "DATE " <> quoted

  forV v = fold [s, " ", op, " ", v]

needUnq :: String -> Boolean
needUnq s = (not $ (show num /= s || isNaN num))
            || s == "true"
            || s == "false"
 where num = readFloat s 

needDate :: String -> Boolean
needDate s = maybe false (const true) $ fromString s


valueToSQL :: Value -> String
valueToSQL (Text v) = v
valueToSQL (Tag v) = v
valueToSQL (Range v v') = "" 

glob2like :: String -> String
glob2like input =
  "'" <> (replace starRgx "%" $ replace questionRgx "_" $ input) <> "'"
  where flags = noFlags{global = true}
        questionRgx = regex "\\?" flags
        starRgx = regex "\\*" flags


labelsProjection :: [String] -> [Label] -> [String]
labelsProjection fields [] = replace firstDot "" <$> fields
labelsProjection _ ls =
  replace firstDot "" <$>
  (foldl (lift2 (<>)) [""] (labelProjection <$> ls))
  
firstDot :: Regex
firstDot = regex "^\\." noFlags

labelProjection :: Label -> [String]
labelProjection (Common "*") = ["{*}", "[*]"]
labelProjection (Common "{*}") = ["{*}"]
labelProjection (Common "[*]") = ["[*]"]
labelProjection (Common l) = ["." <> l]
labelProjection (Meta l) = labelProjection (Common l) 

