-- | file component state update function
module Input.File where

import Control.Alt ((<|>))
import Control.Inject1 (prj)
import Data.Either
import Data.Foldable
import Data.Maybe
import Data.Maybe.Unsafe (fromJust)
import Data.Set (fromList, toList)
import Input.File.Item (inputItem)
import Input.File.Rename (inputRename)
import Input.File.Search (inputSearch)
import Text.SlamSearch (mkQuery)
import Text.SlamSearch.Printer (strQuery)
import qualified Data.Array as A
import qualified Data.List as L
import qualified Data.String as Str
import qualified Model.Breadcrumb as M
import qualified Model.File as M
import qualified Model.Item as M
import qualified Model.Search as M

inner :: M.State -> M.Input -> M.State
inner state input =
  fromJust $ (input1 state <$> prj input)
         <|> ((\i -> state { search = inputSearch state.search i }) <$> prj input)
         <|> ((\i -> state { dialog = flip inputRename i <$> state.dialog }) <$> prj input)
         <|> ((\i -> state { items = inputItem state.sort state.searching state.items i }) <$> prj input)

input1 :: M.State -> M.Input1 -> M.State
input1 state input =
  case input of

    M.Sorting sort ->
      state{sort = sort,
            items = A.sortBy (M.sortItem state.searching sort) state.items}

    M.ItemsUpdate is sort ->
      state{sort = sort,
            items = A.sortBy (M.sortItem state.searching sort) $
                    A.concat [A.filter (\x -> x.root == state.path) $
                              A.filter _.phantom state.items, is]}

    M.SetPath str ->
      state{path = str, breadcrumbs = mkBreadcrumbs str}
    M.Loading loading ->
      state{search = state.search{loading=loading}}
    M.Focus focus ->
      state{search = state.search{focused = focus}}
    M.SetSearching s ->
      state{searching = s}
    M.SetDialog d ->
      state{dialog = d}

  where

        mkBreadcrumbs :: String -> [M.Breadcrumb]
        mkBreadcrumbs path =
          A.reverse <<< L.toArray $
          foldl foldFn (L.Cons M.rootBreadcrumb L.Nil) parts
          where parts = L.fromArray <<< A.filter ((/=) "") $ Str.split "/" path
                foldFn (L.Cons head tail) a =
                  let res = {name: a, link: head.link <> a <> "/"} in
                  L.Cons res (L.Cons head tail)
