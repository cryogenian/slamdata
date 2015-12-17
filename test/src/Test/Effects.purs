module Test.Effects where

import Control.Monad.Aff.AVar (AVAR())
import Control.Monad.Eff.Console (CONSOLE())
import Control.Monad.Eff.Exception (EXCEPTION())
import Control.Monad.Eff.Random (RANDOM())
import Control.Monad.Eff.Ref (REF())
import DOM (DOM())
import Database.Mongo.Mongo (DB())
import Graphics.EasyImage (EASY_IMAGE())
import Graphics.ImageDiff (IMAGE_MAGICK())
import Node.Buffer (BUFFER())
import Node.ChildProcess (CHILD_PROCESS())
import Node.FS (FS())
import Node.Process (PROCESS())
import Platform (PLATFORM())
import Selenium.Types (SELENIUM())
import Test.Env (ENV())

type Effects e =
  ( childProcess :: CHILD_PROCESS
  , err :: EXCEPTION
  , fs :: FS
  , avar :: AVAR
  , process :: PROCESS
  , db :: DB
  , platform :: PLATFORM
  , imageDiff :: IMAGE_MAGICK
  , easyImage :: EASY_IMAGE
  , buffer :: BUFFER
  , random :: RANDOM
  , env :: ENV
  | e)

type SeleniumEffects = Effects ()
type TestEffects = Effects ( ref :: REF
                           , console :: CONSOLE
                           , dom :: DOM
                           , selenium :: SELENIUM
                           )
