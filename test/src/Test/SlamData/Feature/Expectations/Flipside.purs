module Test.SlamData.Feature.Expectations.Flipside where

import SlamData.Prelude
import Test.SlamData.Feature.XPaths as XPaths
import XPath as XPath
import Test.Feature (expectNotPresented, expectPresented)
import Test.SlamData.Feature.Monad (SlamFeature)

flipsideActionNotPresented ∷ String → SlamFeature Unit
flipsideActionNotPresented xPath =
  expectNotPresented
    $ XPath.anywhere
    $ XPath.withPredicate xPath
    $ XPath.not
    $ XPath.attribute "disabled"

flipsideMenuPresented ∷ SlamFeature Unit
flipsideMenuPresented = do
  trashButtonPresented
  publishButtonPresented
  embedButtonPresented

flipsideMenuNotPresented ∷ SlamFeature Unit
flipsideMenuNotPresented = do
  for_
    [ XPaths.trashCardAction
    , XPaths.publishDeckAction
    , XPaths.embedDeckAction
    ]
    flipsideActionNotPresented

embedButtonPresented ∷ SlamFeature Unit
embedButtonPresented =
  expectPresented
    $ XPath.anywhere
    $ XPaths.embedDeckAction

onlyEmbedActionPresented ∷ SlamFeature Unit
onlyEmbedActionPresented = do
  embedButtonPresented
  for_
    [ XPaths.shareDeckAction
    , XPaths.publishDeckAction
    , XPaths.trashCardAction
    ]
    flipsideActionNotPresented

onlyPublishActionPresented ∷ SlamFeature Unit
onlyPublishActionPresented = do
  publishButtonPresented
  for_
    [ XPaths.embedDeckAction
    , XPaths.trashCardAction
    ]
    flipsideActionNotPresented

onlyTrashActionPresented ∷ SlamFeature Unit
onlyTrashActionPresented = do
  trashButtonPresented
  for_
    [ XPaths.publishDeckAction
    , XPaths.embedDeckAction
    ]
    flipsideActionNotPresented

publishButtonPresented ∷ SlamFeature Unit
publishButtonPresented =
  expectPresented
    $ XPath.anywhere
    $ XPaths.publishDeckAction

shareButtonPresented ∷ SlamFeature Unit
shareButtonPresented =
  expectPresented
    $ XPath.anywhere
    $ XPaths.shareDeckAction

trashButtonPresented ∷ SlamFeature Unit
trashButtonPresented =
  expectPresented
    $ XPath.anywhere
    $ XPaths.trashCardAction
