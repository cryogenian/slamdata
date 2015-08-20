module Test.Config where

import Data.StrMap

type Config =
  { selenium :: { browser :: String
                , waitTime :: Int}
  , slamdataUrl :: String
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
  , mount :: { name :: String }
  , version :: String
  }
