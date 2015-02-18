# Module Documentation

## Module Component

### Types

#### `Initial`

    type Initial action state = { state :: state, action :: action }

#### `Receiver`

    
    TODO : Think about Widgets and GlobalComponents and how to
    make functions work with them both. Is it better to use
    row polymorphism or typeclasses

    type Receiver action e = action -> Eff e Unit

#### `RenderFn`

    type RenderFn action state eff = Receiver action eff -> state -> Eff eff VTree

#### `Service`

    type Service action state eff = { send :: Receiver action eff, signal :: Signal state }

#### `UpdateFn`

    type UpdateFn action state eff = action -> state -> Eff eff state

#### `Widget`

    type Widget action state eff = { insert :: Node -> Eff eff Unit, send :: Receiver action eff, signal :: Signal state }

#### `WidgetSpec`

    type WidgetSpec action state eff = { hook :: Receiver action eff -> Eff eff Unit, initial :: Initial action state, updateState :: UpdateFn action state eff, render :: RenderFn action state eff }


### Values

#### `define`

    define :: forall state action e. WidgetSpec action state (dom :: DOM, chan :: Chan | e) -> Eff (dom :: DOM, chan :: Chan | e) (Widget action state (dom :: DOM, chan :: Chan | e))

#### `toVoid`

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

    construct :: forall e. Eff (chan :: Chan | e) (Service Action State _)

#### `foldState`

    foldState :: Action -> State -> Eff _ State

#### `getHashImpl`

    getHashImpl :: forall e. Eff e Hash

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


## Module Router

## Module Utils

### Values

#### `alert`

    alert :: forall e. String -> Eff e Unit

#### `append`

    append :: forall e. Node -> Node -> Eff (dom :: DOM | e) Node

#### `appendToBody`

    appendToBody :: Node -> Eff _ Node

#### `appendToId`

    appendToId :: String -> Node -> Eff _ (Maybe Node)

#### `bodyNode`

    bodyNode :: Eff _ Node

#### `convertToElement`

    convertToElement :: Node -> HTMLElement

#### `convertToNode`

    convertToNode :: HTMLElement -> Node

#### `hashChanged`

    hashChanged :: forall a e. (String -> Eff e Unit) -> Eff e Unit

#### `log`

    log :: forall a e. a -> Eff e Unit

#### `nodeById`

    nodeById :: String -> Eff _ (Maybe Node)

#### `onLoad`

    onLoad :: forall e. Eff e Unit -> Eff e Unit


## Module View

### Types

#### `Action`

    data Action
      = Init 
      | ListAction List.Action
      | NavbarAction Navbar.Action
      | ToolbarAction Toolbar.Action

#### `State`

    type State = { toolbar :: Toolbar.State, list :: List.State, navbar :: Navbar.State }


### Values

#### `spec`

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

    data Action
      = Init 
      | ItemAction Number Item.Action

#### `State`

    type State = { items :: [Item.State] }


### Values

#### `foldState`

    foldState :: Action -> State -> Eff _ State

#### `initialState`

    initialState :: State

#### `view`

    view :: Receiver Action _ -> State -> Eff _ VTree


## Module View.Logo

### Types

#### `Action`

    data Action
      = Init 

#### `State`

    type State = {  }


### Values

#### `foldState`

    foldState :: Action -> State -> Eff _ State

#### `view`

    view :: Receiver Action _ -> State -> Eff _ VTree


## Module View.Navbar

### Types

#### `Action`

    data Action
      = Init 
      | SearchAction Search.Action
      | BackAction Back.Action

#### `State`

    type State = { user :: User.State, back :: Back.State, search :: Search.State }


### Values

#### `foldState`

    foldState :: Action -> State -> Eff _ State

#### `hook`

    hook :: Receiver Action _ -> Eff _ Unit

#### `initialState`

    initialState :: State

#### `view`

    view :: Receiver Action _ -> State -> Eff _ VTree


## Module View.Search

### Types

#### `Action`

    data Action
      = Init 
      | Change String
      | HashChanged String
      | Submit 

#### `State`

    type State = { timeout :: Maybe Timeout, value :: String, valid :: Boolean }


### Values

#### `foldState`

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

    data Action
      = Init 
      | Sorting 
      | UploadFile 
      | MountDB 
      | CreateNotebook 
      | CreateFolder 

#### `Sort`

    data Sort
      = Asc 
      | Desc 

#### `State`

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

    type Handler e = Node -> Event -> Eff e Unit


### Values

#### `getValue`

    getValue :: forall e. Node -> Eff (dom :: DOM | e) String

#### `mkCallback`

    mkCallback :: forall e. Handler e -> Callback

#### `returnFalse`

    returnFalse :: Callback



