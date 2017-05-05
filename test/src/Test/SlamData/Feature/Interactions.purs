module Test.SlamData.Feature.Interactions
       ( module Cards
       , module Chart
       , module Decks
       , module FS
       , module HNG
       , launchSlamData
       )
       where

import Test.SlamData.Feature.Interactions.Cards
       ( addColumn
       , checkFieldInLastDeck
       , doSaveInLastCacheCard
       , expandNewCardMenu
       , filterNextActions
       , provideApiVariableBindingsForVariablesCard
       , provideFieldValueInLastDeck
       , provideMdInLastMdCard
       , provideQueryInLastQueryCard
       , provideSaveDestinationInLastCacheCard
       , provideSearchStringInLastSearchCard
       , pushRadioButtonInLastDeck
       , runQuery
       , selectFileForLastOpenCard
       , selectFromDropdownInLastDeck
       , selectInMillerColumns
       , trashActiveOrLastCard
       , uncheckFieldInLastDeck
       ) as Cards

import Test.SlamData.Feature.Interactions.Chart
       ( selectBuildChart
       , selectInChartBuilder
       , activateCategoryForChartBuilder
       , activateMeasureForChartBuilder
       , activateStackForChartBuilder
       ) as Chart

import Test.SlamData.Feature.Interactions.Deck
       ( accessNextCardInLastDeck
       , accessPreviousCardInLastDeck
       , accessPublishingUrl
       , accessWorkspaceWithModifiedURL
       , confirmDeckAction
       , filterDeckAndCardActions
       , flipDeck
       , insertBuildBarChartCard
       , insertCacheCardInLastDeck
       , insertChartCardInLastDeck
       , insertChartOptionsCardInLastDeck
       , insertDisplayMarkdownCardInLastDeck
       , insertMdCardInLastDeck
       , insertOpenCardInLastDeck
       , insertPivotCard
       , insertQueryCardInLastDeck
       , insertSearchCardInLastDeck
       , insertTableCardInLastDeck
       , insertTroubleshootCardInLastDeck
       , insertVariablesCardInLastDeck
       , nameDeck
       , publishDeck
       , reopenCurrentWorkspace
       , setVarMapForDeck
       , shareDeck
       ) as Decks

import Test.SlamData.Feature.Interactions.FileSystem
       ( accessBreadcrumb
       , accessSharingUrl
       , accessFile
       , browseRootFolder
       , browseTestFolder
       , createFolder
       , createWorkspace
       , createWorkspaceInTestFolder
       , deleteFile
       , deleteFileInTestFolder
       , downloadFileAsCSV
       , downloadFileAsJSON
       , editWorkspace
       , exploreFile
       , hideHiddenFiles
       , mountTestDatabase
       , moveFile
       , moveFileWithClick
       , provideFileSearchString
       , renameFile
       , selectFile
       , shareFile
       , showHiddenFiles
       , uploadFile
       ) as FS

import Test.SlamData.Feature.Interactions.HintsAndGuides
       ( dismissHint
       , skipGuide
       ) as HNG

import SlamData.Prelude

import Selenium.Monad (get)
import Test.SlamData.Feature.Monad (SlamFeature, getConfig)

launchSlamData ∷ SlamFeature Unit
launchSlamData = get ∘ _.slamdataUrl =<< getConfig
