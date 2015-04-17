module Model.Item where

import Data.Maybe
import Control.Timer (Timeout())
import Data.Foreign
import Data.Foreign.Class
import qualified Data.String.Regex as Rgx
import qualified Data.String as Str
import qualified Network.HTTP.Affjax.Request as Ar
import Utils (trimQuotes)


import Model.Sort
import Model.Resource
import Model.Path

-- | Item in list

type Item = {
  selected :: Boolean,
  hovered :: Boolean,
  name :: String,
  resource :: Resource,
  root :: String,
  phantom :: Boolean
  }

itemPath :: Item -> String
itemPath item = (encodeURIPath $
                leadingSlash $ 
                trimQuotes item.root <> trimQuotes item.name) <>
                (if item.resource == Directory || item.resource == Database then "/" else  "")
  where leadingSlash input =
          if Str.indexOf "/" input == 0 then
            input
            else "/" <> input 

-- | link to upper directory
up :: String
up = ".."

-- | default item
initItem :: Item
initItem = {
  resource: Directory,
  hovered: false,
  selected: false,
  root: "",
  name: "",
  phantom: false
  }

-- | upper directory 
upLink :: Item
upLink = initItem{name = ".."}

-- | new directory conf
initDirectory :: Item
initDirectory = initItem{phantom = true}

-- | new notebook item conf
initNotebook :: Item
initNotebook = initItem{resource = Notebook}

-- | new file item conf
initFile :: Item
initFile = initItem{resource = File}

-- | sorting item list with preserving `upItem` in top
-- | If first argument is true then sorting not only by name
-- | but by `item.root <> item.name`
sortItem :: Boolean -> Sort -> Item -> Item -> Ordering
sortItem full dir a b =
  if project a == up
  then LT
  else if project b == up
       then GT
       else case dir of
         Asc -> compare (project a) (project b)
         Desc -> compare (project b) (project a)
  where project =
          if full
          then \x -> x.root <> x.name
          else _.name



