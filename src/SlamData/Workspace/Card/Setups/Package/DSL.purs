module SlamData.Workspace.Card.Setups.Package.DSL
  ( interpret
  , onPrism
  , field
  , addSource
  , isFilteredBy
  , isActiveWhen
  , optional
  , module T
  )where

import SlamData.Prelude

import Control.Monad.Free (liftF, foldFree, hoistFree)
import Control.Monad.State (State, modify, execState, gets)

import Data.Lens (Lens, Prism', view, withPrism, _Just, (%~), (?~), (^.), (.~), (^?))
import Data.List as L
import Data.StrMap as SM

import SlamData.Workspace.Card.Setups.Package.Types as T

hoistField
  ∷ ∀ f ff m
  . (f → ff)
  → (ff → f)
  → T.PackageFF f m
  ~> T.PackageFF ff m
hoistField fn bn = case _ of
  T.DefineField ml p c →
    T.DefineField ml p $ c ∘ bn
  T.Source f a c →
    T.Source (fn f) a $ c ∘ bn
  T.Depends { filter, source } c →
    T.Depends {filter: fn filter, source: fn source} $ c ∘ bn
  T.ActiveGuard { guard, source } c →
    T.ActiveGuard { guard: fn guard, source: fn source } $ c ∘ bn
  T.ModifyField ff f c →
    T.ModifyField (\fld → fn $ ff $ bn fld) (fn f) $ c ∘ bn

indexify ∷ ∀ m. T.PackageFF (T.Field m) m ~> T.PackageFF (Int × (T.Field m)) m
indexify = hoistField (\fld → 0 × fld) snd

fieldF ∷ ∀ m. T.PackageFF (Int × (T.Field m)) m ~> State (L.List (T.Field m))
fieldF = case _ of
  T.DefineField lens projection c → do
    len ← gets L.length
    let fld = T.newField lens projection
    modify $ L.Cons fld
    pure $ c $ len × fld
  T.Source fld a c → do
    modify \st →
      fromMaybe st
        $ L.modifyAt
            (fst fld)
            (T._axesPrjs %~ L.Cons a)
            st
    pure $ c fld
  T.Depends { source, filter } c → do
    modify \st →
      fromMaybe st
        $ L.modifyAt
            (fst source)
            (T._filters %~ L.Cons (snd filter ^. T._projection))
            st
    pure $ c source
  T.ActiveGuard { source, guard } c → do
    modify \st →
      fromMaybe st
        $ L.modifyAt
            (fst source)
            (T._guards %~ L.Cons (snd guard ^. T._projection))
            st
    pure $ c source
  T.ModifyField fn fld@(ix × f) c → do
    let r = fn fld
    modify \st → fromMaybe st $ L.updateAt ix (snd r) st
    pure $ c r

field ∷ ∀ s m a b. Lens s m a b → T.Projection → T.PackageM m (T.Field m)
field l p = liftF $ T.DefineField (T.packAnyLens l) p id

addSource
  ∷ ∀ m a
  . (T.AxisTypeAnnotated a → a)
  → T.Field m
  → T.PackageM m (T.Field m)
addSource prj fld = liftF $ T.Source fld (T.packAxesProjection prj) id

isFilteredBy ∷ ∀ m. T.Field m → T.Field m → T.PackageM m (T.Field m)
isFilteredBy filter source = liftF $ T.Depends { filter, source } id

isActiveWhen ∷ ∀ m. T.Field m → T.Field m → T.PackageM m (T.Field m)
isActiveWhen guard source = liftF $ T.ActiveGuard { source, guard } id

modifyField ∷ ∀ m. (T.Field m → T.Field m) → T.Field m → T.PackageM m (T.Field m)
modifyField fn f = liftF $ T.ModifyField fn f id

optional ∷ ∀ m. (T.Field m) → T.PackageM m (T.Field m)
optional = modifyField $ T._mandatory .~ false

interpret ∷ ∀ m a s. Monoid s ⇒ T.AxesComposer s → T.PackageM m a → T.Package m s
interpret axc pack =
  { allFields
  , cursorMap
  , save
  , load
  }
  where
  fields ∷ L.List (T.Field m)
  fields =
    flip execState L.Nil
    $ foldFree fieldF
    $ hoistFree indexify pack

  allFields ∷ T.DimensionMap → T.AxisTypeAnnotated s → L.List T.Projection
  allFields =
    const $ const $ L.reverse $ map (view T._projection) fields

  cursorMap
    ∷ T.DimensionMap
    → T.AxisTypeAnnotated s
    → SM.StrMap s
  cursorMap dimMap axes =
    let
      foldfn acc f =
        let
          ss = foldMap (\p → spy $ T.unpackAxesProjection p axes) $ f ^. T._axesPrjs
          filtered = foldl (\sm prj → axc.filter prj dimMap sm) ss $ L.reverse $ f ^. T._filters
          guarded = foldl (\sm prj → axc.guard prj dimMap sm) filtered $ L.reverse $ f ^. T._guards
        in acc # T.unpackProjection (f ^. T._projection) ?~ guarded
    in
      foldl foldfn SM.empty fields

  load ∷ Maybe m → T.DimensionMap → T.DimensionMap
  load Nothing state = state
  load (Just m) state =
    let
      -- Unsafe, using _mandatory to determine if it's `Maybe` or not
      foldFn acc fld = acc
        # if fld ^. T._mandatory
          then T.unpackProjection (fld ^. T._projection) ?~ (m ^. T.unpackAnyLens (fld ^. T._lens))
          else T.unpackProjection (fld ^. T._projection) .~ (m ^. T.unpackAnyLens (fld ^. T._lens))
    in
      foldl foldFn state fields

  save ∷ T.DimensionMap → m → Maybe m
  save dimMap m =
    let
      foldfn ∷ m → T.Field m → Maybe m
      foldfn acc fld =
        let
          v = dimMap ^. T.unpackProjection (fld ^. T._projection)
          lns = T.unpackAnyLens (fld ^. T._lens)
        in if not $ fld ^. T._mandatory
           then Just $ acc # T.unpackAnyLens (fld ^. T._lens) .~ v
           else do
             val ← v
             pure $ acc # lns .~ val
    in
     L.foldM foldfn m fields


onPrism ∷ ∀ m n s. Prism' m n → T.Package n s → T.Package m s
onPrism p pack = withPrism p fn
  where
  fn cstr extr =
    pack { save = \dm n → map cstr $ join $ map (pack.save dm) (n ^? p)
         , load = \m dm → pack.load (m ^? _Just ∘ p) dm
         }
