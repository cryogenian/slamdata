module Test.Config where

import Data.StrMap

type Config =
  { selenium :: { browser :: String
                , waitTime :: Int}
  , sauceLabs :: { enabled :: Boolean
                 , platform :: String
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
            , refreshButton :: String
            , playButton :: String
            , embedButton :: String
            , nextCellList :: String
            , cellOutputLabel :: String
            , cellOutputResult :: String
            , failures :: String
            , evalLine :: String
            , showMessages :: String
            , hideMessages :: String
            , statusText :: String
            , embedBox :: String
            , nextCellQuery :: String
            , nextCellSearch :: String
            , nextCellViz :: String
            , nextCellButton :: String
            , exploreFlag :: String
            , searchFlag :: String
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
  , version :: String
  }
