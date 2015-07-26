module Model.Notebook.Search (queryToSQL, needFields) where

import Prelude
import Control.Apply (lift2)
import Data.Either
import Data.List (fromList, List(..))
import Data.Maybe
import Data.Date (fromString)
import Data.Semiring.Free
import Data.Foldable
import Data.String (joinWith, indexOf)
import Data.String.Regex (regex, noFlags, replace, Regex(), test)
import Text.SlamSearch (mkQuery, check)
import Text.SlamSearch.Types
import Global


queryToSQL :: Array String -> SearchQuery -> String
queryToSQL fields query =
  "SELECT" <>
  (if needDistinct whereClause then " DISTINCT " else " ") <>
  "* from {{path}} where " <> whereClause
  where
  whereClause = 
    (joinWith " OR " $
     pars <$> 
     joinWith " AND " <$>
     (fromList <<< (fromList <$>) $
      (runFree $ (termToSQL fields) <$> query)))



needDistinct :: String -> Boolean
needDistinct input = isJust $ indexOf "[*]" input 


needFields :: SearchQuery -> Boolean
needFields query =
  check unit query needFields'
  where
  needFields' :: Unit -> Term -> Boolean
  needFields' _ (Term {labels: Nil}) = true
  needFields' _ _ = false

termToSQL :: Array String -> Term -> String 
termToSQL fields (Term {include: include, predicate: p, labels: ls}) =
  if not include 
  then "NOT " <> (pars $ termToSQL fields $ Term {include: true
                                                 , predicate: p
                                                 , labels: ls})
  else renderPredicate p $ labelsProjection fields (fromList ls)


renderPredicate :: Predicate -> Array String -> String
renderPredicate p prj =
  joinWith " OR " (predicateToSQL p <$> prj)


predicateToSQL :: Predicate -> String -> String
predicateToSQL (Contains (Range v v')) s = range v v' s 
predicateToSQL (Contains (Text v)) s =
  joinWith " OR " $
  ["LOWER(" <> s <> ")" <> " LIKE '%" <> v <> "%'"] <>
  (if needUnq v then render' v else [ ] ) <>
  (if not (needDateTime v) && needDate v then render date else [ ]) <>
  (if needTime v then render time  else [] ) <>
  (if needDateTime v then render ts else [] ) <>
  (if needInterval v then render i else [] )  
  
  where
  quoted = quote v
  date = dated quoted
  time = timed quoted 
  ts = datetimed quoted
  i = intervaled quoted
  render' v = [ "LOWER(" <> s <> ") = " <> v]
  render v = [s <> " = " <> v ]


  
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
  [ forR' quoted quoted' ] <> 
  (if needUnq v && needUnq v' then [ forR v v' ] else [ ]) <>
  (if needDate v && needDate v' then [ forR date date' ] else [ ])
  where
  quoted = quote v
  quoted' = quote v'

  date = dated quoted
  date' = dated quoted'

  forR' :: String -> String -> String
  forR' v v' =
    fold ["(LOWER(", s, ") >=", v, " AND LOWER(", s, ") <= ", v', ")"]
  forR :: String -> String -> String
  forR v v' =
    fold ["(", s, " >= ", v, " AND ", s, " <= ", v', ")"]

qUnQ :: String -> String -> Value -> String
qUnQ s op v = pars $ 
  joinWith " OR " $ 
  [ forV' quoted ] <>
  (if needUnq unquoted then [ forV unquoted ] else [] ) <>
  (if not (needDateTime unquoted) && needDate unquoted then [ forV date ] else [] ) <> 
  (if needTime unquoted then [ forV time ] else [] ) <>
  (if needDateTime unquoted then [ forV ts ] else [] ) <>
  (if needInterval unquoted then [ forV i ] else [] )
  where
  unquoted = valueToSQL v 
  quoted = quote unquoted
  date = dated quoted
  time = timed quoted 
  ts = datetimed quoted 
  i = intervaled quoted

  forV' v = fold ["LOWER(", s, ") ", op, " ", v]
  forV v = fold [s, " ", op, " ", v]

needUnq :: String -> Boolean
needUnq s = (not $ (show num /= s || isNaN num))
            || s == "true"
            || s == "false"
 where num = readFloat s 

needDate :: String -> Boolean
needDate = test dateRegex
  where 
  dateRegex = regex """^(((19|20)([2468][048]|[13579][26]|0[48])|2000)[-]02[-]29|((19|20)[0-9]{2}[-](0[4678]|1[02])[-](0[1-9]|[12][0-9]|30)|(19|20)[0-9]{2}[-](0[1359]|11)[-](0[1-9]|[12][0-9]|3[01])|(19|20)[0-9]{2}[-]02[-](0[1-9]|1[0-9]|2[0-8])))$""" noFlags


needTime :: String -> Boolean
needTime = test timeRegex
  where
  timeRegex = regex "^([0-1]?[0-9]|2[0-3]):[0-5][0-9](:[0-5][0-9])?$" noFlags
  

needDateTime :: String -> Boolean
needDateTime = test dtRegex
  where
  dtRegex = regex "^(-?(?:[1-9][0-9]*)?[0-9]{4})-(1[0-2]|0[1-9])-(3[0-1]|0[1-9]|[1-2][0-9]) (2[0-3]|[0-1][0-9]):([0-5][0-9]):([0-5][0-9])(\\.[0-9]+)?(Z|[+-](?:2[0-3]|[0-1][0-9]):[0-5][0-9])?$" noFlags

needInterval :: String -> Boolean
needInterval = test intervalRegex
  where
  intervalRegex = regex "P((([0-9]*\\.?[0-9]*)Y)?(([0-9]*\\.?[0-9]*)M)?(([0-9]*\\.?[0-9]*)W)?(([0-9]*\\.?[0-9]*)D)?)?(T(([0-9]*\\.?[0-9]*)H)?(([0-9]*\\.?[0-9]*)M)?(([0-9]*\\.?[0-9]*)S)?)?" noFlags


valueToSQL :: Value -> String
valueToSQL (Text v) = v
valueToSQL (Tag v) = v
valueToSQL (Range v v') = "" 

labelsProjection :: Array String -> Array Label -> Array String
labelsProjection fields [] = replace firstDot "" <$> fields
labelsProjection _ ls =
  replace firstDot "" <$>
  (foldl (lift2 (<>)) [""] (labelProjection <$> ls))

labelProjection :: Label -> Array String
labelProjection (Common "*") = ["{*}", "[*]"]
labelProjection (Common "{*}") = ["{*}"]
labelProjection (Common "[*]") = ["[*]"]
labelProjection (Common l) = [".\"" <> l <> "\""]
labelProjection (Meta l) = labelProjection (Common l) 

glob2like :: String -> String
glob2like input =
  quote (replace starRgx "%" $ replace questionRgx "_" $ input)
  where flags = noFlags{global = true}
        questionRgx = regex "\\?" flags
        starRgx = regex "\\*" flags

firstDot :: Regex
firstDot = regex "^\\." noFlags

quote :: String -> String
quote s = "'" <> s <> "'"

pars :: String -> String
pars s = "(" <> s <> ")"

dated :: String -> String
dated s = "DATE " <> s

timed :: String -> String
timed s = "TIME " <> s

datetimed :: String -> String
datetimed s = "TIMESTAMP " <> s

intervaled :: String -> String
intervaled s = "INTERVAL " <> s

