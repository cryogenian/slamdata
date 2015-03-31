-- | state update function 
module Controller.Input where

import Data.Maybe

import qualified Model as M
import qualified Data.Array as A

inner :: M.State -> M.Input -> M.State
inner state input =
  case input of
    M.Remove item ->
      state{items = A.filter (\x -> not $ x.name == item.name && x.root == item.root)
                    state.items}
    -- Used to not call `reload`
    M.Resort ->
      state{items = A.sortBy (M.sortItem  state.sort) state.items}
    M.Sorting sort ->
      state{sort = sort, items = A.sortBy (M.sortItem sort) state.items}
    M.SearchNextValue next ->
      state{search = state.search{nextValue = next}}
    M.BreadcrumbUpdate bs ->
      state{breadcrumbs = bs}
    M.ItemsUpdate is -> 
      state{items = A.concat [A.filter (\x -> x.root == state.path) $
                              A.filter _.phantom state.items, is]}
    M.SearchValidation v ->
      state{search = state.search{valid = v}}
    M.SearchSet s ->
      state{search = state.search{value = s}}
    M.SearchTimeout t ->
      state{search = state.search{timeout = Just t}}
    M.ItemHover ix h ->
      state{items = modify (flip _{hovered = _}) ix}
    M.ItemSelect ix h ->
      state{items = modify (flip _{selected = _}) ix}
    M.SetPath str ->
      state{path = str}
    M.ItemAdd item ->
      state{items = item:state.items}
  where modify func ix =
          let unmodify = func false <$> state.items
              el = func true  <$> unmodify A.!! ix
          in case el of
            Nothing -> unmodify
            Just el -> A.updateAt ix el unmodify
