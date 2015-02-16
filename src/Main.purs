module Main where

import Control.Monad.Eff
import Data.Maybe

import Utils
import qualified Component as Component
import qualified View.Navbar as Navbar


main :: Eff _ Unit
main = onLoad $ do
  navbarComp <- Navbar.construct
  elToInsertNavbar <- nodeById "navbar"
  case elToInsertNavbar of
    Nothing -> log "there is no element to insert navbar"
    Just el -> do
      Component.start navbarComp el



  

  
