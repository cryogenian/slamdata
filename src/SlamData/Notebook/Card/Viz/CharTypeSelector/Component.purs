module SlamData.Notebook.Card.Viz.ChartTypeSelector.Component where

import SlamData.Prelude

import Data.Lens (LensP, lens, (.~))
import Data.Set as Set

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Events.Indexed as HE
import Halogen.Themes.Bootstrap3 as B

import SlamData.Effects (Slam)
import SlamData.Notebook.Card.Chart.ChartType (ChartType(..))
import SlamData.Render.CSS as Rc


type State =
  { variants ∷ Set.Set ChartType
  , selected ∷ ChartType
  }

_variants ∷ ∀ a r. LensP {variants ∷ a|r} a
_variants = lens (_.variants) (_{variants = _})

_selected ∷ ∀ a r. LensP {selected ∷ a|r} a
_selected = lens (_.selected) (_{selected = _})

initialState ∷ State
initialState =
  { variants: Set.empty
  , selected: Pie
  }

data Query a
  = SetVariants (Set.Set ChartType) a
  | Select ChartType a

type DSL = H.ComponentDSL State Query Slam
type HTML = H.ComponentHTML Query


comp ∷ H.Component State Query Slam
comp = H.component { render, eval }

render ∷ State → HTML
render {variants, selected} =
  HH.div
    [ HP.classes [ Rc.vizChartTypeSelector ] ]
    $ foldMap foldFn variants
  where
  foldFn ∷ ChartType → Array HTML
  foldFn ctype =
    [ HH.img
        [ HP.src $ src ctype
        , HP.classes
            $ [ cls ctype ]
            ⊕ (guard (ctype == selected) $> B.active)
        , HE.onClick (HE.input_ (Select ctype))
        ]
    ]
  src ∷ ChartType → String
  src Pie = "img/pie.svg"
  src Line = "img/line.svg"
  src Bar = "img/bar.svg"

  cls ∷ ChartType → HH.ClassName
  cls Pie = Rc.pieChartIcon
  cls Line = Rc.lineChartIcon
  cls Bar = Rc.barChartIcon

eval ∷ Query ~> DSL
eval (SetVariants vs next) = H.modify (_variants .~ vs) $> next
eval (Select v next) = H.modify (_selected .~ v) $> next
