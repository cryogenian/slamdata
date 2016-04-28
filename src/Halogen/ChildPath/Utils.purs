
module Halogen.ChildPath.Utils where

import SlamData.Prelude
import Halogen.Component.ChildPath (ChildPath, cpL, cpR, (:>))

inj1 ∷ ∀ a z. a → a ∨ z
inj1 = Left

inj2 ∷ ∀ a b z. b → a ∨ b ∨ z
inj2 = Right ∘ inj1

inj3 ∷ ∀ a b c z. c → a ∨ b ∨ c ∨ z
inj3 = Right ∘ inj2

inj4 ∷ ∀ a b c d z. d → a ∨ b ∨ c ∨ d ∨ z
inj4 = Right ∘ inj3

inj5 ∷ ∀ a b c d e z. e → a ∨ b ∨ c ∨ d ∨ e ∨ z
inj5 = Right ∘ inj4

inj6 ∷ ∀ a b c d e f z. f → a ∨ b ∨ c ∨ d ∨ e ∨ f ∨ z
inj6 = Right ∘ inj5

inj7 ∷ ∀ a b c d e f g z. g → a ∨ b ∨ c ∨ d ∨ e ∨ f ∨ g ∨ z
inj7 = Right ∘ inj6

inj8 ∷ ∀ a b c d e f g h z. h → a ∨ b ∨ c ∨ d ∨ e ∨ f ∨ g ∨ h ∨ z
inj8 = Right ∘ inj7

prj1 ∷ ∀ a z. a ∨ z → Maybe a
prj1 (Left a) = Just a
prj1 _ = Nothing

prj2 ∷ ∀ a b z. a ∨ b ∨ z → Maybe b
prj2 (Right a) = prj1 a
prj2 _ = Nothing

prj3 ∷ ∀ a b c z. a ∨ b ∨ c ∨ z → Maybe c
prj3 (Right a) = prj2 a
prj3 _ = Nothing

prj4 ∷ ∀ a b c d z. a ∨ b ∨ c ∨ d ∨ z → Maybe d
prj4 (Right a) = prj3 a
prj4 _ = Nothing

prj5 ∷ ∀ a b c d e z. a ∨ b ∨ c ∨ d ∨ e ∨ z → Maybe e
prj5 (Right a) = prj4 a
prj5 _ = Nothing

prj6 ∷ ∀ a b c d e f z. a ∨ b ∨ c ∨ d ∨ e ∨ f ∨ z → Maybe f
prj6 (Right a) = prj5 a
prj6 _ = Nothing

prj7 ∷ ∀ a b c d e f g z. a ∨ b ∨ c ∨ d ∨ e ∨ f ∨ g ∨ z → Maybe g
prj7 (Right a) = prj6 a
prj7 _ = Nothing

prj8 ∷ ∀ a b c d e f g h z. a ∨ b ∨ c ∨ d ∨ e ∨ f ∨ g ∨ h ∨ z → Maybe h
prj8 (Right a) = prj7 a
prj8 _ = Nothing



cinj1
  ∷ ∀ a z u. a u → (a ⨁ z) u
cinj1 = left

cinj2
  ∷ ∀ a b z u. b u → (a ⨁ b ⨁ z) u
cinj2 = right ∘ cinj1

cinj3 ∷ ∀ a b c z u. c u → (a ⨁ b ⨁ c ⨁ z) u
cinj3 = right ∘ cinj2

cinj4 ∷ ∀ a b c d z u. d u → (a ⨁ b ⨁ c ⨁ d ⨁ z) u
cinj4 = right ∘ cinj3

cinj5 ∷ ∀ a b c d e z u. e u → (a ⨁ b ⨁ c ⨁ d ⨁ e ⨁ z) u
cinj5 = right ∘ cinj4

cinj6
  ∷ ∀ a b c d e f z u. f u → (a ⨁ b ⨁ c ⨁ d ⨁ e ⨁ f ⨁ z) u
cinj6 = right ∘ cinj5

cinj7
  ∷ ∀ a b c d e f g z u. g u → (a ⨁ b ⨁ c ⨁ d ⨁ e ⨁ f ⨁ g ⨁ z) u
cinj7 = right ∘ cinj6

cinj8
  ∷ ∀ a b c d e f g h z u. h u → (a ⨁ b ⨁ c ⨁ d ⨁ e ⨁ f ⨁ g ⨁ h ⨁ z) u
cinj8 = right ∘ cinj7

cprj1
  ∷ ∀ a z u. (a ⨁ z) u → Maybe (a u)
cprj1 = coproduct Just (const Nothing)

cprj2
  ∷ ∀ a b z u. (a ⨁ b ⨁ z) u → Maybe (b u)
cprj2 = coproduct (const Nothing) cprj1

cprj3
  ∷ ∀ a b c z u. (a ⨁ b ⨁ c ⨁ z) u → Maybe (c u)
cprj3 = coproduct (const Nothing) cprj2

cprj4
  ∷ ∀ a b c d z u. (a ⨁ b ⨁ c ⨁ d ⨁ z) u → Maybe (d u)
cprj4 = coproduct (const Nothing) cprj3

cprj5
  ∷ ∀ a b c d e z u. (a ⨁ b ⨁ c ⨁ d ⨁ e ⨁ z) u → Maybe (e u)
cprj5 = coproduct (const Nothing ) cprj4

cprj6
  ∷ ∀ a b c d e f z u. (a ⨁ b ⨁ c ⨁ d ⨁ e ⨁ f ⨁ z) u → Maybe (f u)
cprj6 = coproduct (const Nothing) cprj5

cprj7
  ∷ ∀ a b c d e f g z u. (a ⨁ b ⨁ c ⨁ d ⨁ e ⨁ f ⨁ g ⨁ z) u → Maybe (g u)
cprj7 = coproduct (const Nothing) cprj6

cprj8
  ∷ ∀ a b c d e f g h z u. (a ⨁ b ⨁ c ⨁ d ⨁ e ⨁ f ⨁ g ⨁ h ⨁ z) u → Maybe (h u)
cprj8 = coproduct (const Nothing) cprj7

newtype OrdVoid = OrdVoid Void
runOrdVoid ∷ OrdVoid → Void
runOrdVoid (OrdVoid v) = v

instance eqOrdVoid ∷ Eq OrdVoid where
  eq _ = absurd ∘ runOrdVoid

instance ordOrdVoid ∷ Ord OrdVoid where
  compare _ = absurd ∘ runOrdVoid

type Either2 a b = a ∨ b ∨ OrdVoid
type Either3 a b c = a ∨ b ∨ c ∨ OrdVoid
type Either4 a b c d = a ∨ b ∨ c ∨ d ∨ OrdVoid
type Either5 a b c d e = a ∨ b ∨ c ∨ d ∨ e ∨ OrdVoid
type Either6 a b c d e f = a ∨ b ∨ c ∨ d ∨ e ∨ f ∨ OrdVoid
type Either7 a b c d e f g = a ∨ b ∨ c ∨ d ∨ e ∨ f ∨ g ∨ OrdVoid
type Either8 a b c d e f g h = a ∨ b ∨ c ∨ d ∨ e ∨ f ∨ g ∨ h ∨ OrdVoid

type ConstVoid = Const OrdVoid
type Coproduct2 a b = a ⨁ b ⨁ ConstVoid
type Coproduct3 a b c = a ⨁ b ⨁ c ⨁ ConstVoid
type Coproduct4 a b c d = a ⨁ b ⨁ c ⨁ d ⨁ ConstVoid
type Coproduct5 a b c d e = a ⨁ b ⨁ c ⨁ d ⨁ e ⨁ ConstVoid
type Coproduct6 a b c d e f = a ⨁ b ⨁ c ⨁ d ⨁ e ⨁ f ⨁ ConstVoid
type Coproduct7 a b c d e f g = a ⨁ b ⨁ c ⨁ d ⨁ e ⨁ f ⨁ g ⨁ ConstVoid
type Coproduct8 a b c d e f g h = a ⨁ b ⨁ c ⨁ d ⨁ e ⨁ f ⨁ g ⨁ h ⨁ ConstVoid


either2
  ∷ ∀ a b r
  . (a → r) → (b → r)
  → Either2 a b
  → r
either2 a2r b2r = either a2r (either b2r (absurd ∘ runOrdVoid))

either3
  ∷ ∀ a b c r
  . (a → r) → (b → r) → (c → r)
  → Either3 a b c
  → r
either3 a2r b2r c2r = either a2r (either2 b2r c2r)

either4
  ∷ ∀ a b c d r
  . (a → r) → (b → r) → (c → r) → (d → r)
  → Either4 a b c d
  → r
either4 a2r b2r c2r d2r = either a2r (either3 b2r c2r d2r)

either5
  ∷ ∀ a b c d e r
  . (a → r) → (b → r) → (c → r) → (d → r) → (e → r)
  → Either5 a b c d e
  → r
either5 a2r b2r c2r d2r e2r = either a2r (either4 b2r c2r d2r e2r)

either6
  ∷ ∀ a b c d e f r
  . (a → r) → (b → r) → (c → r) → (d → r) → (e → r) → (f → r)
  → Either6 a b c d e f
  → r
either6 a2r b2r c2r d2r e2r f2r =
  either a2r (either5 b2r c2r d2r e2r f2r)

either7
  ∷ ∀ a b c d e f g r
  . (a → r) → (b → r) → (c → r) → (d → r) → (e → r) → (f → r) → (g → r)
  → Either7 a b c d e f g
  → r
either7 a2r b2r c2r d2r e2r f2r g2r =
  either a2r (either6 b2r c2r d2r e2r f2r g2r)

either8
  ∷ ∀ a b c d e f g h r
  . (a → r) → (b → r) → (c → r) → (d → r) → (e → r) → (f → r) → (g → r) → (h → r)
  → Either8 a b c d e f g h
  → r
either8 a2r b2r c2r d2r e2r f2r g2r h2r =
  either a2r (either7 b2r c2r d2r e2r f2r g2r h2r)

coproduct2
  ∷ ∀ a b r u
  . (a u → r) → (b u → r)
  → Coproduct2 a b u
  → r
coproduct2 a2r b2r =
  coproduct a2r (coproduct b2r (absurd ∘ runOrdVoid ∘ getConst))

coproduct3
  ∷ ∀ a b c r u
  . (a u → r) → (b u → r) → (c u → r)
  → Coproduct3 a b c u
  → r
coproduct3 a2r b2r c2r =
  coproduct a2r (coproduct2 b2r c2r)

coproduct4
  ∷ ∀ a b c d r u
  . (a u → r) → (b u → r) → (c u → r) → (d u → r)
  → Coproduct4 a b c d u
  → r
coproduct4 a2r b2r c2r d2r =
  coproduct a2r (coproduct3 b2r c2r d2r)

coproduct5
  ∷ ∀ a b c d e r u
  . (a u → r) → (b u → r) → (c u → r) → (d u → r) → (e u → r)
  → Coproduct5 a b c d e u
  → r
coproduct5 a2r b2r c2r d2r e2r =
  coproduct a2r (coproduct4 b2r c2r d2r e2r)

coproduct6
  ∷ ∀ a b c d e f r u
  . (a u → r) → (b u → r) → (c u → r) → (d u → r) → (e u → r) → (f u → r)
  → Coproduct6 a b c d e f u
  → r
coproduct6 a2r b2r c2r d2r e2r f2r =
  coproduct a2r (coproduct5 b2r c2r d2r e2r f2r)

coproduct7
  ∷ ∀ a b c d e f g r u
  . (a u → r) → (b u → r) → (c u → r) → (d u → r) → (e u → r) → (f u → r) → (g u → r)
  → Coproduct7 a b c d e f g u
  → r
coproduct7 a2r b2r c2r d2r e2r f2r g2r =
  coproduct a2r (coproduct6 b2r c2r d2r e2r f2r g2r)

coproduct8
  ∷ ∀ a b c d e f g h r u
  . (a u → r) → (b u → r) → (c u → r) → (d u → r)
  → (e u → r) → (f u → r) → (g u → r) → (h u → r)
  → Coproduct8 a b c d e f g h u
  → r
coproduct8 a2r b2r c2r d2r e2r f2r g2r h2r =
  coproduct a2r (coproduct7 b2r c2r d2r e2r f2r g2r h2r)

cp1
  ∷ ∀ s t f g p q
  . ChildPath s (s ∨ t) f (f ⨁ g) p (p ∨ q)
cp1 = cpL

cp2
  ∷ ∀ s t u f g h p q r
  . ChildPath t (s ∨ t ∨ u) g (f ⨁ g ⨁ h) q (p ∨ q ∨ r)
cp2 = cpR :> cpL

cp3
  ∷ ∀ a b c d k l m n p q r s
  . ChildPath c (a ∨ b ∨ c ∨ d) m (k ⨁ l ⨁ m ⨁ n) r (p ∨ q ∨ r ∨ s)
cp3 = cpR :> cpR :> cpL
