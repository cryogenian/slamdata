module Model.Item where

import Data.Maybe
import Control.Timer (Timeout())
import Data.Foreign
import Data.Foreign.Class
import qualified Data.String.Regex as Rgx
import qualified Network.HTTP.Affjax.Request as Ar
import Utils (trimQuotes)

import Model.Sort
import Model.Resource


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
itemPath item =
  trimQuotes item.root <> trimQuotes item.name

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
sortItem :: Sort -> Item -> Item -> Ordering
sortItem dir a b =
 let project = _.name
 in if project a == up then LT
    else if project b == up then GT
         else case dir of
           Asc -> compare (project a) (project b)
           Desc -> compare (project b) (project a)

-- | Model for one breadcrumb 
type Breadcrumb = {
  link :: String,
  name :: String
  }

rootBreadcrumb :: Breadcrumb
rootBreadcrumb = {
  name: "root",
  link: "/"
  }

-- | State of search field
type Search = {
  valid :: Boolean,
  value :: String,
  -- if _value_ has been changed but path hasn't been setted
  timeout :: Maybe Timeout,
  -- value to set path
  nextValue :: String
  }

initialSearch :: Search
initialSearch = {
  valid : true,
  value : "",
  timeout : Nothing,
  nextValue: ""
  }


