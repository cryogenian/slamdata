-- | state update function 
module Controller.Input where

import Data.Maybe
import Data.Foldable
import qualified Model as M
import qualified Model.Item as M
import qualified Data.Array as A
import qualified Data.String as Str
import qualified Data.List as L

inner :: M.State -> M.Input -> M.State
inner state input =
  case input of
    M.Remove item ->
      inner state{items = A.filter
                          (\x -> not $ x.name == item.name && x.root == item.root)
                          state.items} M.Resort

    -- Used to not call `reload`
    M.Resort ->
      state{items = A.sortBy (M.sortItem  state.sort) state.items}
    M.Sorting sort ->
      state{sort = sort, items = A.sortBy (M.sortItem sort) state.items}
    M.SearchNextValue next ->
      state{search = state.search{nextValue = next}}
    M.ItemsUpdate is sort -> 
      state{sort = sort,
            items = A.sortBy (M.sortItem sort) $
                    A.concat [A.filter (\x -> x.root == state.path) $
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
      state{path = str, breadcrumbs = mkBreadcrumbs str}
    M.ItemAdd item ->
      inner state{items = item:state.items} M.Resort
      
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


        
