-- | file component state update function
module Input.File where

import Control.Alt ((<|>))
import Control.Inject1 (prj)
import Data.Either
import Data.Foldable
import Data.Maybe
import Data.Maybe.Unsafe (fromJust)
import Data.Set (fromList, toList)
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
inner state input = fromJust $
  (input1 state <$> prj input) <|>
  ((\i -> state { search = inputSearch state.search i }) <$> prj input)

input1 :: M.State -> M.Input1 -> M.State
input1 state input =
  case input of
    M.Remove item ->
      input1 state{items = A.filter
                          (\x -> not $ x.name == item.name && x.root == item.root)
                          state.items} M.Resort

    M.Resort ->
      state{items = A.sortBy (M.sortItem state.searching state.sort) state.items}
    M.Sorting sort ->
      state{sort = sort,
            items = A.sortBy (M.sortItem state.searching sort) state.items}
    M.ItemsUpdate is sort ->
      state{sort = sort,
            items = A.sortBy (M.sortItem state.searching sort) $
                    A.concat [A.filter (\x -> x.root == state.path) $
                              A.filter _.phantom state.items, is]}
    M.ItemHover ix h ->
      state{items = modify (flip _{hovered = _}) ix}
    M.ItemSelect ix h ->
      state{items = modify (flip _{selected = _}) ix}
    M.SetPath str ->
      state{path = str, breadcrumbs = mkBreadcrumbs str}
    M.ItemAdd item ->
      input1 state{items = A.sortBy (M.sortItem state.searching state.sort)
                          (item:state.items)} M.Resort
    M.Loading loading ->
      state{search = state.search{loading=loading}}
    M.Focus focus ->
      state{search = state.search{focused = focus}}
    M.SetSearching s ->
      state{searching = s}
    M.SetDialog d ->
      state{dialog = d}
    M.AddRenameDirs dirs ->
      case state.dialog of
        Just (M.RenameDialog d) ->
          state{dialog = Just (M.RenameDialog d{dirs = A.sort $ unique $ d.dirs <> dirs})}
        _ -> state

    M.SetRenameSelected toSelect ->
      case state.dialog of
        Just (M.RenameDialog d) ->
          state{dialog = Just (M.RenameDialog d{selected = toSelect,
                                                showList = false,
                                                error = ""})}
        _ -> state
    M.RenameChanged newVal ->
      case state.dialog of
        Just (M.RenameDialog d) ->
          state{dialog = Just (M.RenameDialog d{target = newVal})}
        _ -> state
    M.RenameError err ->
      let incorrect = Str.length err /= 0 in
      case state.dialog of
        Just (M.RenameDialog d) ->
          state{dialog = Just (M.RenameDialog d{error = err, incorrect = incorrect})}
        _ -> state

    M.RenameSelectedContent cont ->
      case state.dialog of
        Just (M.RenameDialog d) ->
          state{dialog = Just (M.RenameDialog d{selectedContent = cont})}
        _ -> state




  where modify func ix =
          let unmodify = func false <$> state.items
              el = func true  <$> unmodify A.!! ix
          in case el of
            Nothing -> unmodify
            Just el -> A.updateAt ix el unmodify

        mkBreadcrumbs :: String -> [M.Breadcrumb]
        mkBreadcrumbs path =
          A.reverse <<< L.toArray $
          foldl foldFn (L.Cons M.rootBreadcrumb L.Nil) parts
          where parts = L.fromArray <<< A.filter ((/=) "") $ Str.split "/" path
                foldFn (L.Cons head tail) a =
                  let res = {name: a, link: head.link <> a <> "/"} in
                  L.Cons res (L.Cons head tail)

        unique = toList <<< fromList



