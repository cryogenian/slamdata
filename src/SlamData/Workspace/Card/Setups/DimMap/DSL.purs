module SlamData.Workspace.Card.Setups.DimMap.DSL where

import SlamData.Prelude

import Control.Monad.Free (Free, foldFree, hoistFree, liftF)
import Control.Monad.State (State, modify, execState, gets)

import Data.Lens (view, Lens', lens, (%~), (^.), (?~))
import Data.List as L
import Data.Lens.Iso (Iso', iso, withIso)
import Data.StrMap as SM

import SlamData.Workspace.Card.Setups.Package.Types as PT

data DimensionFF f next
  = MandatoryField PT.Projection (f → next)
  | OptionalField PT.Projection (f → next)
  | Source f PT.AxesProjection (f → next)
  | Depends { filter ∷ f, source ∷ f } (f → next)
  | ActiveGuard { guard ∷ f, source ∷ f } (f → next)

type Field =
  { projection ∷ PT.Projection
  , axesPrjs ∷ L.List PT.AxesProjection
  , guards ∷ L.List PT.Projection
  , filters ∷ L.List PT.Projection
  , optional ∷ Boolean
  }
type DimensionF = DimensionFF Field
type DimensionMF f = Free (DimensionFF f)
type DimensionM = DimensionMF Field

_withZero ∷ ∀ a z. Semiring z ⇒ Iso' a (z × a)
_withZero = iso (zero × _)  snd

_projection ∷ ∀ r a. Lens' { projection ∷ a| r} a
_projection = lens _.projection _{ projection = _ }

_axesPrjs ∷ ∀ r a. Lens' { axesPrjs ∷ a|r } a
_axesPrjs = lens _.axesPrjs _{ axesPrjs = _ }

_guards ∷ ∀ r a. Lens' { guards ∷ a|r } a
_guards = lens _.guards _{ guards = _ }

_filters ∷ ∀ r a. Lens' { filters ∷ a | r } a
_filters = lens _.filters _{ filters = _ }

-- `Iso'` is used because it's more powerful constraint than
-- simple function pair
hoistField
  ∷ ∀ f ff
  . Iso' f ff
  → DimensionFF f
  ~> DimensionFF ff
hoistField iso d = withIso iso \fn bn → case d of
  MandatoryField prj cb →
    MandatoryField prj $ cb ∘ bn
  OptionalField prj cb →
    OptionalField prj $ cb ∘ bn
  Source f a cb →
    Source (fn f) a $ cb ∘ bn
  Depends { filter, source } cb →
    Depends {filter: fn filter, source: fn source } $ cb ∘ bn
  ActiveGuard { guard, source } cb →
    ActiveGuard { guard: fn guard, source: fn source } $ cb ∘ bn

indexify ∷ ∀ f z. Semiring z ⇒ DimensionFF f ~> DimensionFF (z × f)
indexify = hoistField _withZero

type Package s r =
  { allFields ∷ PT.DimensionMap → PT.AxisTypeAnnotated s r → L.List PT.Projection
  , cursorMap ∷ PT.DimensionMap → PT.AxisTypeAnnotated s r → SM.StrMap s
  }

mkField ∷ PT.Projection → Field
mkField projection =
  { projection
  , axesPrjs: L.Nil
  , guards: L.Nil
  , filters: L.Nil
  , optional: false
  }

optionalField ∷ PT.Projection → Field
optionalField = _{ optional = true } ∘ mkField

fieldF ∷ DimensionFF (Int × Field) ~> State (L.List Field)
fieldF = case _ of
  MandatoryField projection c → do
    len ← gets L.length
    let fld = mkField projection
    modify $ L.Cons fld
    pure $ c $ len × fld
  OptionalField projection c → do
    len ← gets L.length
    let fld = optionalField projection
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

field ∷ PT.Projection → DimensionM Field
field p = liftF $ MandatoryField p id

optional ∷ PT.Projection → DimensionM Field
optional p = liftF $ OptionalField p id

addSource ∷ ∀ r s. (PT.AxisTypeAnnotated s r → s) → Field → DimensionM Field
addSource prj fld = liftF $ Source fld (PT.packAxesProjection prj) id

isFilteredBy ∷ Field → Field → DimensionM Field
isFilteredBy filter source = liftF $ Depends { filter, source } id

isActiveWhen ∷ Field → Field → DimensionM Field
isActiveWhen guard source = liftF $ ActiveGuard { source, guard } id

interpret ∷ ∀ a s r. Monoid s ⇒ PT.AxesComposer s → DimensionM a → Package s r
interpret axc cmds =
  { allFields
  , cursorMap
  }
  where
  fields ∷ L.List Field
  fields =
    flip execState L.Nil
    $ foldFree fieldF
    $ hoistFree indexify cmds

  allFields ∷ PT.DimensionMap → PT.AxisTypeAnnotated s r → L.List PT.Projection
  allFields =
    const $ const $ L.reverse $ map (view _projection) fields

  cursorMap
    ∷ PT.DimensionMap
    → PT.AxisTypeAnnotated s r
    → SM.StrMap s
  cursorMap dimMap axes =
    let
      foldfn acc f =
        let
          ss = foldMap (\p → PT.unpackAxesProjection p axes) $ f ^. _axesPrjs
          filtered = foldl (\sm prj → axc.filter prj dimMap sm) ss $ L.reverse $ f ^. _filters
          guarded = foldl (\sm prj → axc.guard prj dimMap sm) filtered $ L.reverse $ f ^. _guards
        in acc # PT.unpackProjection (f ^. _projection) ?~ guarded
    in
      foldl foldfn SM.empty fields
