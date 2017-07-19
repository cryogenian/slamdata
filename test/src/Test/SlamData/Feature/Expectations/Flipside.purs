module Test.SlamData.Feature.Expectations.Flipside where

import SlamData.Prelude

import Test.Feature (expectNotPresented, expectPresented)
import Test.Feature.Log (annotate)
import Test.SlamData.Feature.Monad (SlamFeature)
import Test.SlamData.Feature.XPaths as XPaths
import XPath as XPath

flipsideActionNotPresented ∷ String → SlamFeature Unit
flipsideActionNotPresented xPath =
  expectNotPresented
    $ XPath.anywhere
    $ XPath.withPredicate xPath
    $ XPath.not
    $ XPath.attribute "disabled"

flipsideMenuPresented ∷ SlamFeature Unit
flipsideMenuPresented =
  annotate "Found expected flipside menu" do
    trashButtonPresented
    publishButtonPresented
    embedButtonPresented

flipsideMenuNotPresented ∷ SlamFeature Unit
flipsideMenuNotPresented =
  annotate "Found flipside menu to be not presented as expected" do
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
onlyEmbedActionPresented =
  annotate "Found only Embeded action presented" do
    embedButtonPresented
    for_
      [ XPaths.shareDeckAction
      , XPaths.publishDeckAction
      , XPaths.trashCardAction
      ]
      flipsideActionNotPresented

onlyPublishActionPresented ∷ SlamFeature Unit
onlyPublishActionPresented =
  annotate "Found only Publish action presented" do
    publishButtonPresented
    for_
      [ XPaths.embedDeckAction
      , XPaths.trashCardAction
      ]
      flipsideActionNotPresented

onlyTrashActionPresented ∷ SlamFeature Unit
onlyTrashActionPresented =
  annotate "Found only Trash action presented" do
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
