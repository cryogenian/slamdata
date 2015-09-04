{-
Copyright 2015 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Test.Config
  ( Config(..)
  , SearchQueryConfig(..)
  , ChartOptions(..)
  , platformFromConfig
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.StrMap
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Function (on)
import qualified Data.String.Regex as R
import qualified Test.Platform as P
import qualified Data.Array as A

type SearchQueryConfig =
  { query :: String
  , pages :: Int
  , rows :: Int
  }


newtype ChartOptions = ChartOptions
  { measureOne :: Array String
  , measureTwo :: Array String
  , category :: Array String
  , dimension :: Array String
  , seriesOne :: Array String
  , seriesTwo :: Array String
  }


instance eqChartOptions :: Eq ChartOptions where
  eq (ChartOptions a) (ChartOptions b) =
    eqq a.measureOne b.measureOne
    && eqq a.measureTwo b.measureTwo
    && eqq a.category b.category
    && eqq a.dimension b.dimension
    && eqq a.seriesOne b.seriesOne
    && eqq a.seriesTwo b.seriesTwo
    where
    eqq = on eq A.sort

type Config =
  { selenium :: { browser :: String
                , waitTime :: Int}
  , sauceLabs :: { enabled :: Boolean
                 , platform :: String
                 , maxDuration :: Int
                 }
  , slamdataUrl :: String
  , notebookUrl :: String
  , mongodb :: { host :: String
               , port :: Int
               }

  , locators :: StrMap String
  , notebookLocators :: StrMap String
  , item :: { main :: String
            , toolbar :: String
            }
  , breadcrumbs :: { main :: String
                   , text :: String
                   , home :: String
                   }
  , search :: { searchInput :: String
              , searchButton :: String
              }
  , database :: { name :: String }
  , sort :: { main :: String
            , button :: String}
  , upload :: { file :: String
              , input :: String
              , button :: String
              , filePath :: String
              }
  , move :: { name :: String
            , other :: String
            , markMove :: String
            , button :: String
            , nameField :: String
            , submit :: String
            , markDelete :: String
            }
  , share :: { markShare :: String
             , urlField :: String
             }
  , configureMount :: { nameField :: String
                      , uriField :: String
                      , usernameField :: String
                      , pathField :: String
                      , saveButton :: String
                      , cancelButton :: String
                      , warningBox :: String
                      }
  , modal :: String
  , toolbar :: { main :: String
               , showHide :: String
               , button :: String
               , newFolder :: String
               , newNotebook :: String
               , mountDatabase :: String
               , configureMount :: String
               }
  , mount :: { name :: String
             , otherName :: String
             }
  , newCellMenu :: { expandCollapse :: String
                   , queryButton :: String
                   , mdButton :: String
                   , exploreButton :: String
                   , searchButton :: String
                   }
  , cell :: { main :: String
            , trash :: String
            , hide :: String
            , show :: String
            , exploreEditor :: String
            , searchEditor :: String
            , vizEditor :: String
            , refreshButton :: String
            , playButton :: String
            , embedButton :: String
            , nextCellList :: String
            , nextCellSearch :: String
            , nextCellViz :: String
            , cellOutputLabel :: String
            , cellOutputResult :: String
            , failures :: String
            , evalLine :: String
            , showMessages :: String
            , hideMessages :: String
            , statusText :: String
            , embedBox :: String
            , nextCellsForExplore :: StrMap String
            , nextCellsForSearch :: StrMap String
            , nextCellButton :: String
            , exploreFlag :: String
            , searchFlag :: String
            , mdFlag :: String
            }
  , explore :: { notebookPath :: String
               , input :: String
               , expand :: String
               , list :: String
               , listItem :: String
               , notMounted :: String
               , mounted :: String
               , directory :: String
               , smallZips :: String
               , smallZipsName :: String
               , olympics :: String
               , olympicsName :: String
               , pagination :: String
               , pager :: String
               , pageInput :: String
               , row :: String
               , smallZipsPageCount :: Int
               , olympicsPageCount :: Int
               , initialRowCount :: Int
               , pageSizeInput :: String
               , pageSizeSelect :: String
               , table :: String
               , option :: String
               , optionNums :: Array String
               , optionCustom :: String
               , paginationStepForwardContent :: String
               , paginationFastForwardContent :: String
               , paginationStepBackwardContent :: String
               , paginationFastBackwardContent :: String
               , paginationButtons :: String
               , firstPageContent :: String
               , secondPageContent :: String
               , lastPageContent :: String
               , prenultPageContent :: String
               , customPageContent :: String
               , customPageNumber :: String
               , smallZipsHead :: String
               , olympicsHead :: String
               , nestedHead :: String
               , nestedHeadInversed :: String
               , nested :: String
               , jtableHead :: String
               }
  , searchCell :: { fileListInput :: String
                  , searchInput :: String
                  , searchButton :: String
                  , searchClear :: String
                  , allQuery :: String
                  , incorrectQuery :: String
              }
  , searchQueries :: Array SearchQueryConfig
  , ace :: { textInput :: String
           }
  , query :: { smallZipsAll :: String
             , flatVizAll :: String
             , flatVizMeasures :: String
             }
  , viz :: { heightInput :: String
           , widthInput :: String
           , canvas :: String
           , category :: String
           , measureOne :: String
           , measureTwo :: String
           , seriesOne :: String
           , seriesTwo :: String
           , dimension :: String
           , aggregation :: String
           , barIcon :: String
           , lineIcon :: String
           , pieIcon :: String
           , pieEditor :: String
           , lineEditor :: String
           , barEditor :: String
           , alert :: String
           }
  , vizOptions :: { flatVizAll :: { pie :: ChartOptions
                                  , line :: ChartOptions
                                  , bar :: ChartOptions
                                  }
                  }
  , version :: String
  }

parseSauceLabsPlatform :: String -> P.Platform
parseSauceLabsPlatform str =
  fromMaybe P.Unknown $
    parseByPhrase "Windows" P.Win
      <|> parseByPhrase "OS X" P.Mac
      <|> parseByPhrase "Linux" P.Linux

  where
    parseByPhrase :: String -> P.Platform -> Maybe P.Platform
    parseByPhrase phrase pform =
      if R.test (R.regex phrase R.noFlags) str
         then Just pform
         else Nothing

platformFromConfig :: Config -> P.Platform
platformFromConfig config =
  if config.sauceLabs.enabled
     then parseSauceLabsPlatform config.sauceLabs.platform
     else P.platform

