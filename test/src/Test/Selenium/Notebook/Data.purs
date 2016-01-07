module Test.Selenium.Notebook.Data where

import Prelude
import Data.List (List(), fromFoldable)

fileFromInitialFileList :: String
fileFromInitialFileList = "/test-mount/testDb/cities"

initialFileList :: List String
initialFileList =
  fromFoldable
    [ "/test-mount/"
    , "/test-mount/local/"
    , "/test-mount/local/startup_log"
    , "/test-mount/local/system/"
    , "/test-mount/local/system/indexes"
    , "/test-mount/testDb/"
    , "/test-mount/testDb/Explore.slam/"
    , "/test-mount/testDb/cities"
    , "/test-mount/testDb/flatViz"
    , "/test-mount/testDb/jobs_jobinfo"
    , "/test-mount/testDb/nested"
    , "/test-mount/testDb/nested_foo"
    , "/test-mount/testDb/nulls"
    , "/test-mount/testDb/nullsWithMissing"
    , "/test-mount/testDb/objectids"
    , "/test-mount/testDb/olympics"
    , "/test-mount/testDb/simple"
    , "/test-mount/testDb/smallZips"
    , "/test-mount/testDb/system/"
    , "/test-mount/testDb/unicode"
    , "/test-mount/testDb/usa_factbook"
    , "/test-mount/testDb/user_comments"
    , "/test-mount/testDb/webapp"
    , "/test-mount/testDb/zips"
    , "/test-mount/testDb/.trash/"
    , "/test-mount/testDb/.trash/Explore.slam/"
    ]
