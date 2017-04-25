module SlamData.Header.Attribution where

import SlamData.Prelude
import Data.String as String
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap3 as B
import SlamData.Render.Icon as I

render ∷ ∀ p i . H.Action i → H.HTML p i
render dismiss =
  HH.div
    [ HP.classes [ HH.ClassName "sd-attributions" ] ]
    [ HH.div
        [ HP.classes [ HH.ClassName "deck-dialog-backdrop" ]
        , HE.onMouseDown (HE.input_ dismiss)
        ]
        []
    , HH.div
        [ HP.classes [ HH.ClassName "deck-dialog" ] ]
        [ HH.div_
            [ HH.h4_ [ HH.text "Attributions"]
            , HH.div
                [ HP.classes [ HH.ClassName "deck-dialog-body" ] ]
                attributions
            , HH.div
                [ HP.classes [ HH.ClassName "deck-dialog-footer" ] ]
                [ HH.button
                    [ HP.classes [ B.btn ]
                    , HE.onClick (HE.input_ dismiss)
                    ]
                    [ HH.text "Done" ]
                ]
            ]
        ]
    ]

attributions ∷ ∀ p i. Array (HH.HTML p i)
attributions = I.attributions <#> \(title × names) →
  HH.section_
    [ HH.h5_ [ HH.text title ]
    , HH.p_ [ HH.text (String.joinWith ", " names) ]
    ]
