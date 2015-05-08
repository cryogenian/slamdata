module View.Notebook.Cell.Search where

import Data.Maybe
import Control.Apply (lift2)
import Data.Date (fromString)
import Data.Semiring.Free
import Data.Foldable
import Data.Either 

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Events.Monad as E

import Text.SlamSearch (mkQuery)
import Text.SlamSearch.Types 

import Data.String (joinWith)
import Data.String.Regex (regex, noFlags, replace)
import Global

searchOutput :: forall e i. String -> [ H.HTML (E.Event e i) ]
searchOutput rawInput =
  let input = replace (regex "\\s+" noFlags{global = true}) " " rawInput in
  flip (either (const [])) (mkQuery input) $ \query -> 
  [ H.p_ [ H.text $ "select * from path where " <> (queryToSQL query) ] ]


queryToSQL :: SearchQuery -> String
queryToSQL query =
  joinWith " OR " $
  (\x -> "(" <> x <> ")") <$> 
  joinWith " AND " <$> 
  (runFree $ termToSQL <$> query)


termToSQL :: Term -> String 
termToSQL (Term {include: include, predicate: p, labels: ls}) =
  if not include 
  then "NOT (" <> (termToSQL $ Term {include: true, predicate: p, labels: ls}) <> ")"
  else renderPredicate p $ labelsProjection ls


renderPredicate :: Predicate -> [String] -> String
renderPredicate p prj =
  joinWith " OR " (predicateToSQL p <$> prj)


predicateToSQL :: Predicate -> String -> String
predicateToSQL (Contains (Range v v')) s = range v v' s 
predicateToSQL (Contains (Text v)) s = s <> " LIKE '%" <> v <> "%'"
predicateToSQL (Contains (Tag v)) s = s <> " LIKE '%" <> v <> "%'"
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


labelsProjection :: [Label] -> [String]
labelsProjection [] = ["{*}", "[*]"]
labelsProjection ls =
  replace firstDot "" <$>
  (foldl (lift2 (<>)) [""] (labelProjection <$> ls))
  where firstDot = regex "^\\." noFlags

labelProjection :: Label -> [String]
labelProjection (Common "*") = ["{*}", "[*]"]
labelProjection (Common "{*}") = ["{*}"]
labelProjection (Common "[*]") = ["[*]"]
labelProjection (Common l) = ["." <> l]
labelProjection (Meta l) = labelProjection (Common l) 

