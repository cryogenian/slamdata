module Test.SlamData.Feature.Notebook.Markdown.Interactions where

import Prelude

--import Control.Apply ((*>))
import Test.SlamData.Feature.Monad (SlamFeature())
--import Test.SlamData.Feature.ActionSequence (selectAll, keys)
import Test.SlamData.Feature.Notebook.Interactions (provideMd) --(changeFieldValue, uncheckBox, checkBox, pushRadioButton, selectFromDropdown)
--import Test.SlamData.Feature.Notebook.Interactions (provideMd, provideMdQuery)
--import Test.SlamData.Feature.Notebook.Markdown.Finders (findMdField, findMdQueryField, findMdPlayButton, findMdQueryPlayButton, findCreateMdQueryCellButton)
--import Selenium.Monad (sequence)
--import Data.Traversable (traverse) as T
--import Data.Foldable (traverse_) as F
--
provideMdForFormWithAllInputTypes :: SlamFeature Unit
provideMdForFormWithAllInputTypes =
  provideMd
   """
   discipline = __
   sport = __ (Bobsleigh)
   age = #__
   year = #__ (2002)
   startDate = __ - __ - __
   finishDate = __ - __ - __ (2002-06-06)
   startTime = __ : __
   finishTime = __ : __ (20:39)
   event = {1000m, 1500m, 3000m} (1500m)
   gender = []M []W []X
   color = [x]Red []Green [x]Blue
   type = (x)Gold ()Silver ()Bronze
   """
--
--provideMdForFormWithEvaluatedContent :: SlamFeature (Array Unit)
--provideMdForFormWithEvaluatedContent =
--    T.traverse provideMd
--      [ "discipline = __ (!`SELECT discipline FROM \"/test-mount/testDb/olympics\" LIMIT 1`)"
--      , "year = __ (!`SELECT year FROM \"/test-mount/testDb/olympics\" LIMIT 1`)"
--      , "country = {!`SELECT DISTINCT country FROM \"/test-mount/testDb/olympics\"`} (!`SELECT country FROM \"/test-mount/testDb/olympics\" LIMIT 1`)"
--      , "type = (!`SELECT DISTINCT type FROM \"/test-mount/testDb/olympics\" LIMIT 1`) !`SELECT DISTINCT type FROM \"/test-mount/testDb/olympics\" OFFSET 1`"
--      , "gender = [!`SELECT gender FROM \"/test-mount/testDb/olympics\" LIMIT 1`] !`SELECT DISTINCT gender FROM \"/test-mount/testDb/olympics\"`"
--      ]
--
--provideMdQueryWhichFiltersUsingFormValues :: SlamFeature Unit
--provideMdQueryWhichFiltersUsingFormValues = F.traverse_ provideMdQuery
--  [ "SELECT * FROM \"/test-mount/testDb/olympics\""
--  , "WHERE discipline = :discipline"
--  , "AND type != :type"
--  , "AND gender IN :gender"
--  , "AND year > :year"
--  , "AND country = :country"
--  ]

--changeAllFieldsInMdFormWithEvaluatedContent :: SlamFeature Unit
--changeAllFieldsInMdFormWithEvaluatedContent = do
--  changeFieldValue "discipline" "Luge"
--  changeFieldValue "year" "1950"
--  uncheckBox "W"
--  checkBox "X"
--  checkBox "M"
--  pushRadioButton "Gold"
--  selectFromDropdown "country" "GDR"

