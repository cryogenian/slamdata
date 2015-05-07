module View.Notebook.Cell.Search where

import Data.Semiring.Free
import Data.Foldable
import Data.Either 

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Events.Monad as E

import Text.SlamSearch (mkQuery)
import Text.SlamSearch.Types 

import Debug.Foreign


searchOutput :: forall e i. String -> [ H.HTML (E.Event e i) ]
searchOutput input =
  flip (either (const [])) (mkQuery input) $ \query -> 
  [ H.p_ [ H.text $ "select * from path where " <> (queryToSQL query) ] ]


queryToSQL :: SearchQuery -> String
queryToSQL query =
  foldl (\a b -> a <> " OR " <> b) "false" $
  (\x -> "(" <> x <> ")") <$> 
  foldl (\a b -> a <> " AND " <> b) "true" <$>
  (runFree $ termToSQL <$> query)


termToSQL :: Term -> String 
termToSQL (Term {include: include, predicate: p, labels: ls}) =
  if not include 
  then "NOT " <> (termToSQL $ Term {include: true, predicate: p, labels: ls})
  else renderPredicate p $ labelsProjection ls 


