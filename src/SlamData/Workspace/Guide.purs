{-
Copyright 2016 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module SlamData.Workspace.Guide where

import SlamData.Prelude

import SlamData.Guide.StepByStep.Component (Step)

data GuideType
  = CardGuide
  | FlipGuide

derive instance eqGuideSlot ∷ Eq GuideType
derive instance ordGuideSlot ∷ Ord GuideType

cardGuideSteps ∷ Array Step
cardGuideSteps =
  [ { imageUri: "img/cardGuide/1.gif"
    , text: "Welcome to SlamData! When using SlamData we think about analytics in terms of cards."
    }
  , { imageUri: "img/cardGuide/2.gif"
    , text: "Each card performs a function like showing a pie chart or opening a data set."
    }
  , { imageUri: "img/cardGuide/3.gif"
    , text: "Each card passes an output to the card after it, for example passing a data set to a chart."
    }
  , { imageUri: "img/cardGuide/4.gif"
    , text: "This is done by stacking cards ontop of each other to build decks. Decks represent analytic workflows."
    }
  , { imageUri: "img/cardGuide/5.gif"
    , text: "Check out the deck flip side, slide the card grippers and visit the next action card to see more of what's possible with SlamData."
    }
  ]

flipGuideSteps ∷ Array Step
flipGuideSteps =
  [ { imageUri: "img/flipGuide/1.gif"
    , text: "Welcome to the flip side of your deck. The flip side lists actions for the deck and current card."
    }
  , { imageUri: "img/flipGuide/2.gif"
    , text: "Delete Card deletes the current card and any cards inserted after it. Error cards and next action cards aren't inserted so are not deleted."
    }
  , { imageUri: "img/flipGuide/3.gif"
    , text: "Wrap Deck places the deck on a dashboard card in a new deck."
    }
  , { imageUri: "img/flipGuide/4.gif"
    , text: "Dashboard cards allow muliple decks to be laid out on one card."
    }
  , { imageUri: "img/flipGuide/5.gif"
    , text: "Decks can be removed using Delete Deck."
    }
  , { imageUri: "img/flipGuide/6.gif"
    , text: "Mirror Deck creates a live copy of the cards in the deck and places them in a new deck on the current dashboard card."
    }
  , { imageUri: "img/flipGuide/7.gif"
    , text: "Any changes made to the original or mirrored cards are reflected in each other except for deletion."
    }
  , { imageUri: "img/flipGuide/8.gif"
    , text: "Adding cards to either deck will not affect the other. Mirroring is great for adding form controls to your dashboard."
    }
  , { imageUri: "img/flipGuide/9.gif"
    , text: "Sharing is easy and secure with SlamData Advanced."
    }
  , { imageUri: "img/flipGuide/10.gif"
    , text: "Share deck allows decks to be shared for viewing or editing with people and groups. It also allows for the creation of access tokens."
    }
  , { imageUri: "img/flipGuide/11.gif"
    , text: "Access can be managed or revoked using Unshare Deck"
    }
  , { imageUri: "img/flipGuide/12.gif"
    , text: "With both SlamData and SlamData Advanced decks can be embedded on web and intranet sites using Embed Deck."
    }
  , { imageUri: "img/flipGuide/13.gif"
    , text: "And public viewing links can be created with Publish Deck."
    }
  , { imageUri: "img/flipGuide/14.gif"
    , text: "Thanks for exploring your Deck's flip side with us. If you get stuck or want to learn more please check out the help menu."
    }
  ]

dismissedCardGuideKey ∷ String
dismissedCardGuideKey = "dismissedCardGuide"

dismissedFlipGuideKey ∷ String
dismissedFlipGuideKey = "dismissedFlipGuide"

dismissedFocusDeckHintKey ∷ String
dismissedFocusDeckHintKey = "dismissedFocusDeckHint"

dismissedFocusDeckFrameHintKey ∷ String
dismissedFocusDeckFrameHintKey = "dismissedFocusDeckFrameHint"
