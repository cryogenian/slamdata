module SlamData.Dialog.Share.User.Component where

import Prelude

import Control.MonadPlus (guard)

import Data.Lens (LensP(), lens, (<>~), (.~), (%~))
import Data.Lens.Index (ix)
import Data.Functor (($>))
import Data.Foldable as F
import Data.Tuple as Tpl
import Data.Array as Arr
import Data.Maybe as M
import Data.Map as Map

import DOM.HTML.Types (HTMLElement())

import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B
import Halogen.CustomProps as Cp

import SlamData.Effects (Slam())
import SlamData.Render.CSS as Rc
import SlamData.Render.Common (glyph)

import Utils.Array (enumerate)

type State =
  {
    inputs :: Map.Map Int String
  }

_inputs :: forall a r. LensP {inputs :: a|r} a
_inputs = lens _.inputs _{inputs = _}

initialState :: State
initialState =
  {
    inputs: Map.empty
  }

data Query a
  = InputChanged Int String a
  | Remove Int a
  | Blured Int a
  | Create Int a
  | Filter a
  | Focused Int a

type UserShareDSL = ComponentDSL State Query Slam

comp :: Component State Query Slam
comp = component render eval

render :: State -> ComponentHTML Query
render state =
  H.form
    [ Cp.nonSubmit
    , P.classes [ Rc.userShareForm ]
    ]
    ((F.foldMap (Tpl.uncurry showInput) $ Map.toList state.inputs)
     <> [ newInput $ newKey state.inputs]
    )
  where
  newKey :: forall a. Map.Map Int a ->  Int
  newKey m = M.fromMaybe zero $ add one $ F.maximum $ Map.keys m

  showInput :: Int -> String -> Array (ComponentHTML Query)
  showInput inx val =
    [ H.div [ P.classes [ B.inputGroup ] ]
      [
        H.input [ P.classes [ B.formControl ]
                , P.value val
                , ARIA.label "User email or name"
                , E.onValueInput (E.input (InputChanged inx))
                , E.onBlur (E.input_ (Blured inx))
                , E.onFocus (E.input_ (Focused inx))
                , P.key $ show inx
                ]
      , H.span [ P.classes [ B.inputGroupBtn ] ]
        [ H.button
          [ P.classes [ B.btnDefault, B.btn ]
          , E.onClick (E.input_ (Remove inx))
          , P.buttonType P.ButtonButton
          , ARIA.label "Clear user email or name"
          ]
          [ glyph B.glyphiconRemove ]
        ]
      ]
     ]

  newInput :: Int -> ComponentHTML Query
  newInput inx =
    H.div [ P.classes [ B.inputGroup ] ]
      [
        H.input [ P.classes [ B.formControl ]
                , ARIA.label "User email or name"
                , E.onFocus (E.input_ (Create inx))
                , P.value ""
                , P.key $ show inx
                ]
      , H.span [ P.classes [ B.inputGroupBtn ] ]
        [ H.button
          [ P.classes [ B.btn ]
          , P.disabled true
          , P.buttonType P.ButtonButton
          , ARIA.label "Clear user email or name"
          ]
          [ glyph B.glyphiconRemove ]
        ]
      ]


eval :: Natural Query UserShareDSL
eval (Create inx next) = modify (_inputs %~ Map.insert inx "") $> next
eval (InputChanged inx val next) = modify (_inputs <<< ix inx .~ val) $> next
eval (Remove inx next) =
  modify (_inputs %~ Map.delete inx) $> next
eval (Focused inx next) = do
  modify (_inputs %~ Map.update (\x -> guard (x /= "") $> x) inx)

--  modify (_inputs %~ (M.fromMaybe [ ] <<< Arr.deleteAt inx)) $> next
  pure next
eval (Filter next) =
  pure next
--  modify (_inputs %~ Arr.delete "") $> next
eval (Blured i next) = do
  pure next
--  Debug.Trace.traceAnyA i
--  modify (_inputs %~ Arr.delete "") $> next
