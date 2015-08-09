module Test.Config where

import Data.StrMap

type Config = 
  { selenium :: { browser :: String
                , waitTime :: Int}
  , slamdataUrl :: String
    
  , locators :: StrMap String
  , item :: { main :: String
            , toolbar :: String
            }
  , breadcrumbs :: { main :: String
                   , text :: String
                   , home :: String
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
  , modal :: String
  , toolbar :: { main :: String
               , showHide :: String
               , button :: String
               , newFolder :: String
               , newNotebook :: String
               } 
  , version :: String
  }
