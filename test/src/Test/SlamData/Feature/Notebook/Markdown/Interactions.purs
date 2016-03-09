module Test.SlamData.Feature.Notebook.Markdown.Interactions where

import Data.String (joinWith)
import Prelude
import Test.Feature (provideFieldValue, selectFromDropdown, pushRadioButton, check, uncheck)
import Test.SlamData.Feature.Monad (SlamFeature())
import Test.SlamData.Feature.Notebook.Interactions (provideMd, provideMdQuery)

provideMdForFormWithAllInputTypes :: SlamFeature Unit
provideMdForFormWithAllInputTypes =
  provideMd $ joinWith "\n\n" $
    [ "discipline = __"
    , "sport = __ (Bobsleigh)"
    , "age = #__"
    , "year = #__ (2002)"
    , "startDate = __ - __ - __"
    , "finishDate = __ - __ - __ (2002-06-06)"
    , "startTime = __ : __"
    , "finishTime = __ : __ (20:39)"
    , "event = {1000m, 1500m, 3000m} (1500m)"
    , "gender = []M []W []X"
    , "color = [x]Red []Green [x]Blue"
    , "type = (x)Gold ()Silver ()Bronze"
    ]

provideMdForFormWithEvaluatedContent :: SlamFeature Unit
provideMdForFormWithEvaluatedContent =
  provideMd $ joinWith "\n\n" $
    [ "discipline = __ (!``SELECT discipline FROM `/test-mount/testDb/olympics` LIMIT 1``)"
    , "year = __ (!``SELECT year FROM `/test-mount/testDb/olympics` LIMIT 1``)"
    , "country = {!``SELECT DISTINCT country FROM `/test-mount/testDb/olympics` ``} (!``SELECT country FROM `/test-mount/testDb/olympics` LIMIT 1``)"
    , "type = (!``SELECT DISTINCT type FROM `/test-mount/testDb/olympics` LIMIT 1``) !``SELECT DISTINCT type FROM `/test-mount/testDb/olympics` OFFSET 1``"
    , "gender = [!``SELECT gender FROM `/test-mount/testDb/olympics` LIMIT 1``] !``SELECT DISTINCT gender FROM `/test-mount/testDb/olympics` ``"
    ]

provideMdQueryWhichFiltersUsingFormValues :: SlamFeature Unit
provideMdQueryWhichFiltersUsingFormValues =
  provideMdQuery
    "SELECT * FROM `/test-mount/testDb/olympics` WHERE discipline = :discipline AND type != :type AND gender IN :gender AND year > :year AND country = :country"

changeAllFieldsInMdFormWithEvaluatedContent :: SlamFeature Unit
changeAllFieldsInMdFormWithEvaluatedContent = do
  provideFieldValue (XPath.anywhere $ "input" `XPath.withLabelWithExactText` "discipline") "Luge"
  provideFieldValue (XPath.anywhere $ "input" `XPath.withLabelWithExactText` "year") "1950"
  uncheck $ XPath.anywhere $ "input" `XPath.withLabelWithExactText` "W"
  check $ XPath.anywhere $ "input" `XPath.withLabelWithExactText` "X"
  check $ XPath.anywhere $ "input" `XPath.withLabelWithExactText` "M"
  pushRadioButton $ XPath.anywhere $ "input" `XPath.withLabelWithExactText` "Gold"
  selectFromDropdown (XPath.anywhere $ "select" `XPath.withLabelWithExactText` "country") "GDR"
