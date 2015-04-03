module Model.Item where

import Data.Maybe
import Control.Timer (Timeout())
import Data.Foreign
import Data.Foreign.Class
import qualified Data.String.Regex as Rgx
import qualified Network.HTTP.Affjax.Request as Ar

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

-- | Will be `DecodeJson` instance or `Json -> Maybe Item`
-- | after switching to `affjax`
readItem :: Foreign -> F Item
readItem f = do
  name <- readProp "name" f
  resource <- readProp "type" f
  pure $
    if isNotebook name && resource == File then
      {
        selected: false,
        hovered: false,
        name: name,
        resource: Notebook,
        root: "",
        phantom: false
      }
      else
       {
         selected: false,
         hovered: false,
         name: name,
         resource: resource,
         root: "",
         phantom: false
       }
  where isNotebook name = name /=
                          Rgx.replace (Rgx.regex "\\.nb$" Rgx.noFlags) "" name

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
upLink = initItem{name = up}

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


