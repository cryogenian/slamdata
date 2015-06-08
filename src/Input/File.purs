module Input.File
  ( Input()
  , FileInput(..)
  , updateState
  ) where

import Control.Alt ((<|>))
import Data.Either
import Data.Foldable
import Data.Inject1 (prj, inj)
import Data.Maybe
import Data.Maybe.Unsafe (fromJust)
import Data.Path.Pathy
import Data.Set (fromList, toList)
import Input.File.Item (ItemInput(), inputItem)
import Input.File.Mount (MountInput(), inputMount)
import Input.File.Rename (RenameInput(), inputRename)
import Input.File.Search (SearchInput(), inputSearch)
import Model.Breadcrumb (Breadcrumb(), rootBreadcrumb)
import Model.File (State())
import Model.File.Dialog (Dialog())
import Model.File.Item (Item(), sortItem)
import Model.Path
import Model.Resource
import Model.Salt (Salt())
import Model.Sort (Sort())
import Text.SlamSearch (mkQuery)
import Text.SlamSearch.Printer (strQuery)

import qualified Data.Array as A
import qualified Data.List as L
import qualified Data.String as Str


type Input = Either ItemInput (Either FileInput (Either SearchInput (Either RenameInput MountInput)))

data FileInput
  = Sorting Sort
  | SetPath DirPath
  | Loading Boolean
  | Focus Boolean
  | SetSearching Boolean
  | SetSalt Salt
  | SetDialog (Maybe Dialog)
  | SetHasMountRoot Boolean

updateState :: State -> Input -> State
updateState state input =
  fromJust $ (inputFile state <$> prj input)
         <|> ((\i -> state { items = inputItem state.sort state.searching state.items i }) <$> prj input)
         <|> ((\i -> state { search = inputSearch state.search i }) <$> prj input)
         <|> ((\i -> state { dialog = flip inputRename i <$> state.dialog }) <$> prj input)
         <|> ((\i -> state { dialog = flip inputMount i <$> state.dialog }) <$> prj input)

inputFile :: State -> FileInput -> State
inputFile state input =
  case input of
    Sorting sort ->
      state { sort = sort
            , items = A.sortBy (sortItem state.searching sort) state.items}
    SetPath p ->
      state{path = p, breadcrumbs = mkBreadcrumbs p}
    Loading loading ->
      state{search = state.search{loading=loading}}
    Focus focus ->
      state{search = state.search{focused = focus}}
    SetSearching s ->
      state{searching = s}
    SetSalt s ->
      state{salt = s}
    SetDialog d ->
      state{dialog = d}
    SetHasMountRoot b ->
      state{hasMountRoot = b}


  where
  mkBreadcrumbs :: DirPath -> [Breadcrumb]
  mkBreadcrumbs path =
    A.reverse <<< L.toArray $
    foldl foldFn (L.Cons rootBreadcrumb L.Nil) parts
    where parts = L.fromArray <<< A.filter ((/=) "") $ Str.split "/" $ printPath path
          foldFn (L.Cons head tail) a =
            let res = {name: a, link: head.link <> a <> "/"} in
            L.Cons res (L.Cons head tail)
