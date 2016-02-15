module Test.Selenium.Notebook.Markdown.Interactions where

import Prelude

import Control.Apply ((*>))
import Test.Selenium.Monad (Check(), getConfig, getModifierKey)
import Test.Selenium.ActionSequence (selectAll, keys, focus)
import Test.Selenium.Notebook.Contexts (makeCell)
import Test.Selenium.Interactions (changeFieldValue, uncheckBox, checkBox, pushRadioButton, selectFromDropdown)
import Selenium.ActionSequence (leftClick)
import Test.Selenium.Notebook.Markdown.Finders (findMdField, findMdQueryField, findMdPlayButton, findMdQueryPlayButton, findCreateMdQueryCellButton)
import Selenium.Monad (sequence)

import Data.Traversable (traverse) as T
import Data.Foldable (traverse_) as F

provideMd :: String -> Check Unit
provideMd md = focusMdField *> sequence (keys $ md ++ " ")

focusMdField :: Check Unit
focusMdField = findMdField >>= focus >>> sequence

changeMd :: String -> Check Unit
changeMd md = do
  focusMdField
  modifierKey <- getModifierKey
  sequence $ selectAll modifierKey *> keys md

playMd :: Check Unit
playMd = findMdPlayButton >>= sequence <<< leftClick

playMdQuery :: Check Unit
playMdQuery = findMdQueryPlayButton >>= sequence <<< leftClick

provideMdForFormWithAllInputTypes :: Check (Array Unit)
provideMdForFormWithAllInputTypes =
  T.traverse provideMd
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


provideMdForFormWithEvaluatedContent :: Check (Array Unit)
provideMdForFormWithEvaluatedContent =
    T.traverse provideMd
      [ "discipline = __ (!`SELECT discipline FROM `/test-mount/testDb/olympics` LIMIT 1`)"
      , "year = __ (!`SELECT year FROM `/test-mount/testDb/olympics` LIMIT 1`)"
      , "country = {!`SELECT DISTINCT country FROM `/test-mount/testDb/olympics``} (!`SELECT country FROM `/test-mount/testDb/olympics` LIMIT 1`)"
      , "type = (!`SELECT DISTINCT type FROM `/test-mount/testDb/olympics` LIMIT 1`) !`SELECT DISTINCT type FROM `/test-mount/testDb/olympics` OFFSET 1`"
      , "gender = [!`SELECT gender FROM `/test-mount/testDb/olympics` LIMIT 1`] !`SELECT DISTINCT gender FROM `/test-mount/testDb/olympics` `"
      ]

createMdQueryCell :: Check Unit
createMdQueryCell = findCreateMdQueryCellButton >>= leftClick >>> sequence

focusMdQueryField :: Check Unit
focusMdQueryField = findMdQueryField >>= focus >>> sequence

provideMdQuery :: String -> Check Unit
provideMdQuery query = focusMdQueryField *> (sequence $ keys $ query ++ " ")

provideMdQueryWhichFiltersUsingFormValues :: Check Unit
provideMdQueryWhichFiltersUsingFormValues = F.traverse_ provideMdQuery
  [ "SELECT * FROM `/test-mount/testDb/olympics`"
  , "WHERE discipline = :discipline"
  , "AND type != :type"
  , "AND gender IN :gender"
  , "AND year > :year"
  , "AND country = :country"
  ]

changeAllFieldsInMdFormWithEvaluatedContent :: Check Unit
changeAllFieldsInMdFormWithEvaluatedContent = do
  changeFieldValue "discipline" "Luge"
  changeFieldValue "year" "1950"
  uncheckBox "W"
  checkBox "X"
  checkBox "M"
  pushRadioButton "Gold"
  selectFromDropdown "country" "GDR"

insertMdCell :: Check Unit
insertMdCell = getConfig >>= _.newCellMenu >>> _.mdButton >>> makeCell
