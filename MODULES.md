# Module Documentation

## Module Component

### Types

#### `Initial`

This is just for reducing number of fields in spec 

    type Initial action state = { state :: state, action :: action }

#### `Receiver`

    
    TODO : Think about Widgets and GlobalComponents and how to
    make functions work with them both. Is it better to use
    row polymorphism or typeclasses
     | This is type of message receiver in component
     | It takes an **action** and do something with it

    type Receiver action e = action -> Eff e Unit

#### `RenderFn`

this function modifies virtual tree 

    type RenderFn action state eff = Receiver action eff -> state -> Eff eff VTree

#### `Service`

WIP : This is component that has no render function
I think it must be global in some cases.
I suggest to use it for FileUploader, ApiCalls and Router

    type Service action state eff = { send :: Receiver action eff, signal :: Signal state }

#### `UpdateFn`

this function fold **state** with **action**

    type UpdateFn action state eff = action -> state -> Eff eff state

#### `Widget`

This is basic component that can be rendered to DOM

    type Widget action state eff = { insert :: Node -> Eff eff Unit, send :: Receiver action eff, signal :: Signal state }

#### `WidgetSpec`

Spec of component

    type WidgetSpec action state eff = { hook :: Receiver action eff -> Eff eff Unit, initial :: Initial action state, updateState :: UpdateFn action state eff, render :: RenderFn action state eff }


### Values

#### `define`

Takes **WidgetSpec** returns **Widget**

    define :: forall state action e. WidgetSpec action state (dom :: DOM, chan :: Chan | e) -> Eff (dom :: DOM, chan :: Chan | e) (Widget action state (dom :: DOM, chan :: Chan | e))

#### `toVoid`

Shortcut to void receiver

    toVoid :: forall a e. Receiver a e


## Module Hash

### Types

#### `Action`

    type Action = String

#### `Hash`

    type Hash = String

#### `State`

    type State = String


### Values

#### `construct`

constructing service

    construct :: forall e. Eff (chan :: Chan | e) (Service Action State (chan :: Chan | e))

#### `foldState`

If we have a message to set hash we just set hash

    foldState :: Action -> State -> Eff _ State

#### `getHashImpl`

    getHashImpl :: forall e. Eff e Hash

#### `initialState`

    initialState :: Eff _ State

#### `matcher`

    matcher :: Regex

#### `onHashChange`

    onHashChange :: forall e a. Eff e Unit -> Eff e Unit

#### `setHash`

    setHash :: forall e. String -> Eff e Unit

#### `setHashImpl`

    setHashImpl :: forall e. String -> Eff e Unit


## Module Main

### Values

#### `main`

    main :: Eff _ Unit


## Module Model

### Types

#### `Sort`

    data Sort
      = Asc 
      | Desc 


### Values

#### `sortFromString`

    sortFromString :: String -> Maybe Sort

#### `sortToString`

    sortToString :: Sort -> String


## Module Router

### Types

#### `Action`

Incoming messages can be
HashChanged is external message

    data Action
      = Init 
      | Search String
      | Sorting Sort
      | HashChanged String

#### `State`

state of routing is sort direction and search string

    type State eff = { hash :: Service Hash.Action Hash.State eff, search :: String, sort :: Sort }


### Values

#### `construct`

    construct :: Eff _ (Service Action (State _) _)

#### `extractSearch`

Get search from hash

    extractSearch :: String -> String

#### `extractSort`

Get sort from hash

    extractSort :: String -> Maybe Sort

#### `foldState`

Updating router state by messages

    foldState :: forall e. Action -> State (chan :: Chan | e) -> Eff (chan :: Chan | e) (State (chan :: Chan | e))

#### `fromHash`

Make state without hash-component from hash-string

    fromHash :: String -> { search :: String, sort :: Sort }

#### `initialState`

make initial state 

    initialState :: Eff _ (State _)

#### `setSearch`

shortcut for setting search string
I think, that after some other services will be
developed pattern will arise

    setSearch :: String -> Eff _ Unit

#### `toHash`

Make hash

    toHash :: forall o. { search :: String, sort :: Sort | o } -> String


## Module Utils

### Values

#### `alert`

Used for mocking somewhere :)

    alert :: forall e. String -> Eff e Unit

#### `append`

append one node to another node
need to rewrite to HTMLElement
and make PR to simple-dom

    append :: forall e. Node -> Node -> Eff (dom :: DOM | e) Node

#### `appendToBody`

Shortcut

    appendToBody :: Node -> Eff _ Node

#### `appendToId`

Shortcut : will be removed

    appendToId :: String -> Node -> Eff _ (Maybe Node)

#### `bodyNode`

Shortcut

    bodyNode :: Eff _ Node

#### `convertToElement`

Will be removed

    convertToElement :: Node -> HTMLElement

#### `convertToNode`

Will be removed

    convertToNode :: HTMLElement -> Node

#### `hashChanged`

need to PR to simple-dom

    hashChanged :: forall a e. (String -> Eff e Unit) -> Eff e Unit

#### `log`

It's simpler for me to use foreign logging
then add Show instances

    log :: forall a e. a -> Eff e Unit

#### `nodeById`

Shortcut

    nodeById :: String -> Eff _ (Maybe Node)

#### `onLoad`

This function can be defined via
purescript-simple-dom but it will be
harder to understand

    onLoad :: forall e. Eff e Unit -> Eff e Unit


## Module View

### Types

#### `Action`

Action is sum of children actions

    data Action
      = Init 
      | ListAction List.Action
      | NavbarAction Navbar.Action
      | ToolbarAction Toolbar.Action

#### `State`

State is multiplication of children state

    type State = { toolbar :: Toolbar.State, list :: List.State, navbar :: Navbar.State }


### Values

#### `spec`

Spec 

    spec :: WidgetSpec Action State _


## Module Signal.Effectful

### Values

#### `foldpE`

    foldpE :: forall a b c e. (a -> b -> Eff e b) -> b -> Signal a -> Eff e (Signal b)


## Module View.Back

### Types

#### `Action`

    data Action
      = Init 
      | Clicked 
      | Changed State

#### `State`

    data State
      = Directory 
      | Database 
      | Table 
      | Notebook 


### Values

#### `foldState`

    foldState :: Action -> State -> Eff _ State

#### `view`

    view :: Receiver Action _ -> State -> Eff _ VTree

#### `viewIcon`

    viewIcon :: State -> VTree


## Module View.Item

### Types

#### `Action`

    data Action
      = Init 
      | Focus 
      | Blur 
      | Open 
      | Activate 
      | Unactivate 

#### `State`

    type State = { isHovered :: Boolean, isSelected :: Boolean }


### Values

#### `foldState`

    foldState :: Action -> State -> Eff _ State

#### `initialState`

    initialState :: State

#### `view`

    view :: Receiver Action _ -> State -> Eff _ VTree


## Module View.List

### Types

#### `Action`

External messages will be marked with index of child
that send it

    data Action
      = Init 
      | ItemAction Number Item.Action

#### `State`

Its state has a list of children

    type State = { items :: [Item.State] }


### Values

#### `foldState`

    foldState :: Action -> State -> Eff _ State

#### `initialState`

    initialState :: State

#### `view`

Rendering of list

    view :: Receiver Action _ -> State -> Eff _ VTree


## Module View.Logo

### Values

#### `view`

send and st will be removed

    view :: forall a b. a -> b -> Eff _ VTree


## Module View.Navbar

### Types

#### `Action`

Sum of children actions

    data Action
      = Init 
      | SearchAction Search.Action
      | BackAction Back.Action

#### `State`

Multiplication of children state

    type State = { user :: User.State, back :: Back.State, search :: Search.State }


### Values

#### `foldState`

Update state

    foldState :: Action -> State -> Eff _ State

#### `hook`

listen route changes, called after render

    hook :: forall e. Receiver Action (ref :: Ref, chan :: Chan | e) -> Eff (ref :: Ref, chan :: Chan | e) Unit

#### `initialState`

    initialState :: State

#### `view`

Render

    view :: Receiver Action _ -> State -> Eff _ VTree


## Module View.Search

### Types

#### `Action`

Route change is external message

    data Action
      = Init 
      | Change String
      | RouteChanged String
      | Submit 

#### `State`

    type State = { timeout :: Maybe Timeout, value :: String, valid :: Boolean }


### Values

#### `foldState`

Update searcher state

    foldState :: Action -> State -> Eff _ State

#### `initialState`

    initialState :: State

#### `view`

    view :: Receiver Action _ -> State -> Eff _ VTree


## Module View.Shortcuts

### Values

#### `a`

    a :: forall props. {  | props } -> [VTree] -> VTree

#### `button`

    button :: forall props. {  | props } -> [VTree] -> VTree

#### `div`

    div :: forall props. {  | props } -> [VTree] -> VTree

#### `emptyVTree`

useful for defining of components

    emptyVTree :: VTree

#### `form`

    form :: forall props. {  | props } -> [VTree] -> VTree

#### `i`

    i :: forall props. {  | props } -> [VTree] -> VTree

#### `input`

    input :: forall props. {  | props } -> [VTree] -> VTree

#### `jsVoid`

    jsVoid :: String

#### `li`

    li :: forall props. {  | props } -> [VTree] -> VTree

#### `nav`

    nav :: forall props. {  | props } -> [VTree] -> VTree

#### `ol`

    ol :: forall props. {  | props } -> [VTree] -> VTree

#### `span`

    span :: forall props. {  | props } -> [VTree] -> VTree

#### `ul`

    ul :: forall props. {  | props } -> [VTree] -> VTree


## Module View.Toolbar

### Types

#### `Action`

Output messages

    data Action
      = Init 
      | Sorting 
      | UploadFile 
      | MountDB 
      | CreateNotebook 
      | CreateFolder 

#### `State`

sort direction in list (used in chevron direction right now only)

    type State = { sort :: Sort }


### Values

#### `foldState`

    foldState :: Action -> State -> Eff _ State

#### `view`

    view :: Receiver Action _ -> State -> Eff _ VTree


## Module View.User

### Types

#### `Action`

    data Action
      = Init 

#### `State`

    newtype State
      = State String


### Values

#### `foldState`

    foldState :: Action -> State -> Eff _ State

#### `view`

    view :: Receiver Action _ -> State -> Eff _ VTree


## Module VirtualDOM.Events

### Types

#### `Callback`

    data Callback :: *

#### `Event`

    data Event :: *

#### `Handler`

Will be changed in favor of HTMLElement, I suppose

    type Handler e = Node -> Event -> Eff e Unit


### Values

#### `getValue`

Just get value from **input**

    getValue :: forall e. Node -> Eff (dom :: DOM | e) String

#### `mkCallback`

    mkCallback :: forall e. Handler e -> Callback

#### `returnFalse`

emptyHandler

    returnFalse :: Callback



