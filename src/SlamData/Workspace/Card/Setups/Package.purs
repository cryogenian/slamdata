module SlamData.Workspace.Card.Setups.Package where

import SlamData.Prelude

import Control.Monad.Free (Free, liftF, foldFree, hoistFree)
import Control.Monad.State (State, modify, execState, gets)

import Data.Lens (Lens, Lens', Prism', lens, view, (.~), (^.), (?~), withPrism, (^?), _Just, (%~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.List as L
import Data.Monoid.Endo (Endo(..))
import Data.StrMap as SM

import SlamData.Workspace.Card.Setups.Common.State (Projection)
import SlamData.Workspace.Card.Setups.Axis (AxisTypeAnnotated)
import SlamData.Workspace.Card.Setups.Common.State as C
import SlamData.Workspace.Card.Model as M

import Unsafe.Coerce (unsafeCoerce)

type Package m s =
  { allFields ∷ L.List Projection
  , cursorMap ∷ C.DimensionMap → AxisTypeAnnotated s → SM.StrMap s
  , save ∷ C.DimensionMap → m → Maybe m
  , load ∷ Maybe m → C.DimensionMap → C.DimensionMap
  }

data AnyLens m

packAnyLens ∷ ∀ s m a b. Lens s m a b → AnyLens m
packAnyLens = unsafeCoerce

unpackAnyLens ∷ ∀ s m a b. AnyLens m → Lens s m a b
unpackAnyLens = unsafeCoerce

data AxesProjection

packAxesProjection ∷ ∀ s. (AxisTypeAnnotated s → s) → AxesProjection
packAxesProjection = unsafeCoerce

unpackAxesProjection ∷ ∀ s. AxesProjection → AxisTypeAnnotated s → s
unpackAxesProjection = unsafeCoerce

newtype Field m = Field
  { projection ∷ Projection
  , mandatory ∷ Boolean
  , lens ∷ AnyLens m
  , axesPrjs ∷ L.List AxesProjection
  , guards ∷ L.List Projection
  , filters ∷ L.List Projection
  }

derive instance newtypeField ∷ Newtype (Field m) _

_projection ∷ ∀ m. Lens' (Field m) Projection
_projection = _Newtype ∘ lens _.projection _{ projection = _ }

_mandatory ∷ ∀ m. Lens' (Field m) Boolean
_mandatory = _Newtype ∘ lens _.mandatory _{ mandatory = _ }

_lens ∷ ∀ m. Lens' (Field m) (AnyLens m)
_lens = _Newtype ∘ lens _.lens _{ lens = _ }

_axesPrjs ∷ ∀ m. Lens' (Field m) (L.List AxesProjection)
_axesPrjs = _Newtype ∘ lens _.axesPrjs _{ axesPrjs = _ }

_guards ∷ ∀ m. Lens' (Field m) (L.List Projection)
_guards = _Newtype ∘ lens _.guards _{ guards = _ }

_filters ∷ ∀ m. Lens' (Field m) (L.List Projection)
_filters = _Newtype ∘ lens _.filters _{ filters = _ }

data PackageFF f m next
  = DefineField (AnyLens m) Projection (f → next)
  | Source f AxesProjection (f → next)
  | Depends { filter ∷ f, source ∷ f } (f → next)
  | ActiveGuard { guard ∷ f, source ∷ f } (f → next)
  | ModifyField (f → f) f (f → next)

type PackageF m = PackageFF (Field m) m

type PackageMF f m = Free (PackageFF f m)

type PackageM m = Free (PackageF m)

onPrism ∷ ∀ m n s. Prism' m n → Package n s → Package m s
onPrism p pack = withPrism p fn
  where
  fn cstr extr =
    pack { save = \dm n → map cstr $ join $ map (pack.save dm) (n ^? p)
         , load = \m dm → pack.load (m ^? _Just ∘ p) dm
         }

type AxesComposer s =
  { guard ∷ Projection → s → s
  , filter ∷ Projection → s → s
  }

build
  ∷ ∀ m a s
  . Monoid s
  ⇒ AxesComposer s
  → PackageM m a
  → Package m s
build axc pack =
  { allFields
  , cursorMap
  , save
  , load
  }
  where
  fields ∷ L.List (Field m)
  fields =
    flip execState L.Nil
    $ foldFree fieldF
    $ hoistFree indexify pack

  allFields ∷ L.List Projection
  allFields =
    L.reverse $ map (view _projection) fields

  cursorMap
    ∷ C.DimensionMap
    → AxisTypeAnnotated s
    → SM.StrMap s
  cursorMap dimMap axes =
    let
      foldfn acc f =
        let
          ss = foldMap (\p → unpackAxesProjection p axes) $ f ^. _axesPrjs
          filtered = foldl (flip axc.filter) ss $ L.reverse $ f ^. _filters
          guarded = foldl (flip axc.guard) filtered $ L.reverse $ f ^. _guards
        in acc # C.unpack (f ^. _projection) ?~ guarded
    in
      foldl foldfn SM.empty fields

  load ∷ Maybe m → C.DimensionMap → C.DimensionMap
  load Nothing state = state
  load (Just m) state =
    let
      foldFn fld = Endo $ C.unpack (fld ^. _projection) .~ (m ^. unpackAnyLens (fld ^. _lens))
      modifier = unwrap $ foldMap foldFn fields
    in
     modifier state

  save ∷ C.DimensionMap → m → Maybe m
  save dimMap m =
    let
      foldfn ∷ m → Field m → Maybe m
      foldfn acc fld =
        let
          v = dimMap ^. C.unpack (fld ^. _projection)
          lns = unpackAnyLens (fld ^. _lens)
        in if not $ fld ^. _mandatory
           then Just $ acc # unpackAnyLens (fld ^. _lens) .~ v
           else do
             val ← v
             pure $ acc # lns .~ val
    in
     L.foldM foldfn m fields

hoistField
  ∷ ∀ f ff m
  . (f → ff)
  → (ff → f)
  → PackageFF f m
  ~> PackageFF ff m
hoistField fn bn = case _ of
  DefineField ml p c →
    DefineField ml p $ c ∘ bn
  Source f a c →
    Source (fn f) a $ c ∘ bn
  Depends { filter, source } c →
    Depends {filter: fn filter, source: fn source} $ c ∘ bn
  ActiveGuard { guard, source } c →
    ActiveGuard { guard: fn guard, source: fn source } $ c ∘ bn
  ModifyField ff f c →
    ModifyField (\fld → fn $ ff $ bn fld) (fn f) $ c ∘ bn

indexify
  ∷ ∀ m
  . PackageFF (Field m) m
  ~>  PackageFF (Int × (Field m)) m
indexify =
  hoistField (\fld → 0 × fld) snd

fieldF
  ∷ ∀ m
  . PackageFF (Int × (Field m)) m
  ~> State (L.List (Field m))
fieldF = case _ of
  DefineField lens projection c → do
    len ← gets L.length
    let
      fld = Field
        { mandatory: true
        , projection
        , lens
        , axesPrjs: L.Nil
        , guards: L.Nil
        , filters: L.Nil
        }
    modify $ L.Cons fld
    pure $ c $ len × fld
  Source fld a c → do
    modify \st →
      fromMaybe st
        $ L.modifyAt
            (fst fld)
            (_axesPrjs %~ L.Cons a)
            st
    pure $ c fld
  Depends { source, filter } c → do
    modify \st →
      fromMaybe st
        $ L.modifyAt
            (fst source)
            (_filters %~ L.Cons (snd filter ^. _projection))
            st
    pure $ c source
  ActiveGuard { source, guard } c → do
    modify \st →
      fromMaybe st
        $ L.modifyAt
            (fst source)
            (_guards %~ L.Cons (snd guard ^. _projection))
            st
    pure $ c source
  ModifyField fn fld@(ix × f) c → do
    let r = fn fld
    modify \st → fromMaybe st $ L.updateAt ix (snd r) st
    pure $ c r



field ∷ ∀ s m a b. Lens s m a b → Projection → PackageM m (Field m)
field l p = liftF $ DefineField (packAnyLens l) p id

addSource ∷ ∀ m. (AxisTypeAnnotated (Array Unit) → Array Unit) → Field m → PackageM m (Field m)
addSource prj fld = liftF $ Source fld (packAxesProjection prj) id

isFilteredBy ∷ ∀ m. Field m → Field m → PackageM m (Field m)
isFilteredBy source filter = liftF $ Depends { filter, source } id

isActiveWhen ∷ ∀ m. Field m → Field m → PackageM m (Field m)
isActiveWhen source guard = liftF $ ActiveGuard { source, guard } id

workWith ∷ ∀ m n a. Prism' m n → PackageM n a → PackageM m a
workWith = unsafeCoerce

modifyField ∷ ∀ m. (Field m → Field m) → Field m → PackageM m (Field m)
modifyField fn f = liftF $ ModifyField fn f id

optional ∷ ∀ m. (Field m) → PackageM m (Field m)
optional = modifyField $ _mandatory .~ false

_dimension ∷ ∀ a b r. Lens { dimension ∷ a | r } { dimension ∷ b | r } a b
_dimension = lens _.dimension _{ dimension = _ }

_value ∷ ∀ a b r. Lens { value ∷ a | r } { value ∷ b | r } a b
_value = lens _.value _{ value = _ }

_series ∷ ∀ a b r. Lens { series ∷ a | r } { series ∷ b | r } a b
_series = lens _.series _{ series = _ }

_axisLabelAngle ∷ ∀ a b r. Lens { axisLabelAngle ∷ a | r } { axisLabelAngle ∷ b | r } a b
_axisLabelAngle = lens _.axisLabelAngle _{ axisLabelAngle = _ }

_isSmooth ∷ ∀ a b r. Lens { isSmooth ∷ a | r } { isSmooth ∷ b | r } a b
_isSmooth = lens _.isSmooth _{ isSmooth = _ }

_isStacked ∷ ∀ a b r. Lens { isStacked ∷ a | r } { isStacked ∷ b | r } a b
_isStacked = lens _.isStacked _{ isStacked = _ }


composer ∷ ∀ s. Monoid s ⇒ AxesComposer s
composer =
  { filter: \p a → a
  , guard: \p a → a
  }

barPackage ∷ ∀ s. Monoid s ⇒ Package M.AnyCardModel s
barPackage = onPrism (M._BuildArea ∘ _Just) $ build composer do
  dimension ← field _dimension C._dimension
  addSource _.category dimension

  value ← field _value C._value
  addSource _.value value
  value `isFilteredBy` dimension

  series ← field _series C._series
    >>= optional
    >>= addSource (\ax → ax.category ⊕ ax.time ⊕ ax.date )
    >>= isActiveWhen dimension
    >>= isFilteredBy dimension
    >>= isFilteredBy value

  pure unit
