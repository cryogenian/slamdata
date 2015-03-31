# Module Documentation

## Module App

#### `app`

``` purescript
app :: forall e. Hl.UI M.Input Void M.Request (timer :: Tm.Timer | e)
```



## Module Config

#### `uploadUrl`

``` purescript
uploadUrl :: String
```


#### `metadataUrl`

``` purescript
metadataUrl :: String
```


#### `dataUrl`

``` purescript
dataUrl :: String
```


#### `notebookUrl`

``` purescript
notebookUrl :: String
```


#### `searchTimeout`

``` purescript
searchTimeout :: Number
```


#### `slamDataHome`

``` purescript
slamDataHome :: String
```


#### `userEnabled`

``` purescript
userEnabled :: Boolean
```


#### `newNotebookName`

``` purescript
newNotebookName :: String
```


#### `newFolderName`

``` purescript
newFolderName :: String
```



## Module Main


## Module Model


Input, output messages and state

#### `Input`

``` purescript
data Input
  = Sorting Sort
  | BreadcrumbUpdate [Breadcrumb]
  | ItemsUpdate [Item]
  | ItemHover Number Boolean
  | ItemSelect Number Boolean
  | ItemAdd Item
  | SearchValidation Boolean
  | SearchSet String
  | SearchTimeout Timeout
  | SearchNextValue String
  | SetPath String
  | Resort 
  | Remove Item
```

Input messages 

#### `Request`

``` purescript
data Request
  = GoToRoute String
  | SetSort Sort
  | SearchChange (Maybe Timeout) String
  | Breadcrumb Breadcrumb
  | SearchSubmit Search
  | Open Item
  | Delete Item
  | Share Item
  | Move Item
  | Configure Item
  | CreateNotebook State
  | MountDatabase State
  | CreateFolder State
  | UploadFile Node State
  | FileListChanged Node State
```

Request Messages 

#### `Sort`

``` purescript
data Sort
  = Asc 
  | Desc 
```

Sort direction

#### `notSort`

``` purescript
notSort :: Sort -> Sort
```

revese sort

#### `sort2string`

``` purescript
sort2string :: Sort -> String
```


#### `string2sort`

``` purescript
string2sort :: String -> Maybe Sort
```


#### `eqSort`

``` purescript
instance eqSort :: Eq Sort
```


#### `Resource`

``` purescript
data Resource
  = File 
  | Database 
  | Notebook 
  | Directory 
  | Table 
```

Resource tag for item 

#### `eqResource`

``` purescript
instance eqResource :: Eq Resource
```


#### `resource2str`

``` purescript
resource2str :: Resource -> String
```


#### `resourceIsForeign`

``` purescript
instance resourceIsForeign :: IsForeign Resource
```

Now only `IsForeign`. After switching to `purescript-affjax`
will be `EncodeJson`

#### `Item`

``` purescript
type Item = { phantom :: Boolean, root :: String, resource :: Resource, name :: String, hovered :: Boolean, selected :: Boolean }
```

Item in list

#### `readItem`

``` purescript
readItem :: Foreign -> F Item
```

Will be `DecodeJson` instance or `Json -> Maybe Item`
after switching to `affjax`

#### `up`

``` purescript
up :: String
```

link to upper directory

#### `initItem`

``` purescript
initItem :: Item
```

default item

#### `upLink`

``` purescript
upLink :: Item
```

upper directory

#### `initDirectory`

``` purescript
initDirectory :: Item
```

new directory conf

#### `initNotebook`

``` purescript
initNotebook :: Item
```

new notebook item conf

#### `initFile`

``` purescript
initFile :: Item
```

new file item conf

#### `sortItem`

``` purescript
sortItem :: Sort -> Item -> Item -> Ordering
```

sorting item list with preserving `upItem` in top

#### `Breadcrumb`

``` purescript
type Breadcrumb = { name :: String, link :: String }
```

Model for one breadcrumb 

#### `rootBreadcrumb`

``` purescript
rootBreadcrumb :: Breadcrumb
```


#### `Search`

``` purescript
type Search = { nextValue :: String, timeout :: Maybe Timeout, value :: String, valid :: Boolean }
```

State of search field

#### `initialSearch`

``` purescript
initialSearch :: Search
```


#### `State`

``` purescript
type State = { path :: String, breadcrumbs :: [Breadcrumb], items :: [Item], sort :: Sort, search :: Search }
```

Application state

#### `initialState`

``` purescript
initialState :: State
```


#### `CellType`

``` purescript
data CellType
  = Evaluate 
  | Explore 
  | Search 
  | Query 
  | Visualize 
  | Markdown 
```

Notebook cell type

#### `CellMetadata`

``` purescript
type CellMetadata = {  }
```


#### `Cell`

``` purescript
type Cell = { metadata :: CellMetadata, cellType :: CellType, output :: String, input :: String }
```

Cell model

#### `NbMetadata`

``` purescript
type NbMetadata = {  }
```


#### `Notebook`

``` purescript
type Notebook = { cells :: [Cell], metadata :: NbMetadata }
```


#### `newNotebook`

``` purescript
newNotebook :: Notebook
```



## Module Utils


Shortcuts and glue between different modules

#### `encodeURIComponent`

``` purescript
encodeURIComponent :: String -> String
```

#### `decodeURIComponent`

``` purescript
decodeURIComponent :: String -> String
```


#### `log`

``` purescript
log :: forall a e. a -> Eff (trace :: Trace | e) Unit
```


#### `onLoad`

``` purescript
onLoad :: forall e. Eff (dom :: DOM | e) Unit -> Eff (dom :: DOM | e) Unit
```


#### `append`

``` purescript
append :: forall e. HTMLElement -> HTMLElement -> Eff (dom :: DOM | e) HTMLElement
```


#### `newTab`

``` purescript
newTab :: forall e. String -> Eff (dom :: DOM | e) Unit
```

Opens url in new tab or window

#### `currentHash`

``` purescript
currentHash :: forall e. Eff e String
```

gets current hash

#### `convertToElement`

``` purescript
convertToElement :: Node -> HTMLElement
```

converts `Node` to `HTMLElement`

#### `reload`

``` purescript
reload :: forall e. Eff e Unit
```



## Module View

#### `view`

``` purescript
view :: forall u node. (H.HTMLRepr node) => M.State -> node u (Either M.Input M.Request)
```



## Module Api.Fs


This module will be reworked after `affjax` is ready.

#### `metadata`

``` purescript
metadata :: forall e. String -> ([M.Item] -> Eff (dom :: DOM | e) Unit) -> Eff (dom :: DOM | e) Unit
```

gets current directory children

#### `makeNotebook`

``` purescript
makeNotebook :: forall e. String -> M.Notebook -> (Boolean -> Eff (dom :: DOM | e) Unit) -> Eff (dom :: DOM | e) Unit
```


#### `makeFile`

``` purescript
makeFile :: forall e. String -> String -> (Boolean -> Eff (dom :: DOM | e) Unit) -> Eff (dom :: DOM | e) Unit
```


#### `move`

``` purescript
move :: forall e. String -> String -> (Boolean -> Eff (dom :: DOM | e) Unit) -> Eff (dom :: DOM | e) Unit
```


#### `deleteItem`

``` purescript
deleteItem :: forall e. M.Item -> (Boolean -> Eff (dom :: DOM | e) Unit) -> Eff (dom :: DOM | e) Unit
```

delete item


## Module Controller.Driver


Module handles outer messages to `halogen` application
Mostly consists of routing functions 

#### `outside`

``` purescript
outside :: forall e. Hl.Driver M.Input (timer :: Tm.Timer | e) -> Eff (Hl.HalogenEffects (timer :: Tm.Timer | e)) Unit
```

Entry, used in `halogen` app

#### `searchPath`

``` purescript
searchPath :: S.SearchQuery -> Maybe String
```

Extract path predicate from search query

#### `SortSetter`

``` purescript
data SortSetter
  = SortSetter M.Sort
```

Set _sort_ in route

#### `routeModifierSortSetter`

``` purescript
instance routeModifierSortSetter :: RS.RouteModifier SortSetter
```


#### `QSetter`

``` purescript
data QSetter
  = QSetter String
```

Set _q_ in route

#### `routeModifierQSetter`

``` purescript
instance routeModifierQSetter :: RS.RouteModifier QSetter
```


#### `routeSetterSortSetter`

``` purescript
instance routeSetterSortSetter :: RS.RouteState SortSetter
```


#### `getPath`

``` purescript
getPath :: String -> String
```

extract path from full hash

#### `PathSetter`

``` purescript
newtype PathSetter
  = PathSetter String
```

set _path_ value in path-labeled predicate in route

#### `routeModifierPathSetter`

``` purescript
instance routeModifierPathSetter :: RS.RouteModifier PathSetter
```


#### `PathAdder`

``` purescript
newtype PathAdder
  = PathAdder String
```

Add to _path_ value in path-labeled predicate 

#### `routeModifierPathAdder`

``` purescript
instance routeModifierPathAdder :: RS.RouteModifier PathAdder
```



## Module Controller.Input


state update function 

#### `inner`

``` purescript
inner :: M.State -> M.Input -> M.State
```



## Module Controller.Request


Main app handler

#### `handler`

``` purescript
handler :: forall e. M.Request -> Aff.Aff (Hl.HalogenEffects (timer :: Tm.Timer | e)) M.Input
```



## Module Utils.Event

#### `raiseEvent`

``` purescript
raiseEvent :: forall e a. String -> HTMLElement -> a -> Eff (dom :: DOM | e) HTMLElement
```



## Module Utils.File

#### `FileReader`

``` purescript
data FileReader :: *
```


#### `File`

``` purescript
data File :: *
```


#### `FileList`

``` purescript
data FileList :: *
```


#### `fileListToArray`

``` purescript
fileListToArray :: FileList -> [File]
```


#### `name`

``` purescript
name :: forall e. File -> Eff e String
```


#### `file2blob`

``` purescript
file2blob :: File -> Blob
```


#### `newFormData`

``` purescript
newFormData :: forall e. Eff e FormData
```


#### `append2FormData`

``` purescript
append2FormData :: forall e a. String -> a -> FormData -> Eff e FormData
```


#### `newReader`

``` purescript
newReader :: forall e. Eff e FileReader
```


#### `readAsBinaryString`

``` purescript
readAsBinaryString :: forall e. File -> FileReader -> Eff e Unit
```


#### `str2blob`

``` purescript
str2blob :: forall e. String -> Blob
```


#### `resultImpl`

``` purescript
resultImpl :: forall e a. Fn3 (Maybe a) (a -> Maybe a) FileReader (Eff e (Maybe String))
```


#### `result`

``` purescript
result :: forall e. FileReader -> Eff e (Maybe String)
```


#### `files`

``` purescript
files :: forall e. HTMLElement -> Eff e FileList
```


#### `onload`

``` purescript
onload :: forall e e'. FileReader -> Eff e Unit -> Eff e' FileReader
```


#### `clearValue`

``` purescript
clearValue :: forall e. HTMLElement -> Eff e Unit
```



## Module Utils.Halide

#### `back`

``` purescript
back :: forall e i r. i -> EventHandler (Either i r)
```


#### `request`

``` purescript
request :: forall e i r. r -> EventHandler (Either i r)
```




