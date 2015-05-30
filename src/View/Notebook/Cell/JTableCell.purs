module View.Notebook.Cell.JTableCell (renderJTableOutput) where

import Control.Functor (($>))
import Controller.Notebook.Cell.JTableContent
import Controller.Notebook.Common (I())
import Data.Array (elemIndex)
import Data.Char (fromCharCode)
import Data.Either (either, isLeft)
import Data.Int (Int(), toNumber)
import Data.Json.JTable (renderJTable, jTableOptsDefault, bootstrapStyle)
import Data.Maybe (fromMaybe)
import Data.String (fromChar)
import Data.These (These(), these, theseRight)
import Data.Void (absurd)
import Model.Notebook.Cell (Cell())
import Model.Notebook.Cell.JTableContent (JTableContent(), _result, _page, _perPage, _values, _totalPages)
import Optic.Core ((^.), (..))
import Optic.Extended (TraversalP(), (^?))
import Optic.Refractor.Prism (_Just)
import View.Common (glyph)
import View.Notebook.Common (HTML())

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Forms as E
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.Themes.Bootstrap3 as B
import qualified View.Css as VC

renderJTableOutput :: forall e. TraversalP Cell JTableContent -> (Cell -> I e) -> Cell -> [HTML e]
renderJTableOutput lens run cell = fromMaybe [] $ do
  table <- cell ^? lens
  result <- table ^? _result .. _Just
  json <- result ^? _values .. _Just
  let totalPages = result ^. _totalPages
      page = fromMaybe one $ theseRight (table ^. _page)
      output = renderJTable (jTableOptsDefault { style = bootstrapStyle }) json
      pageSizeValue = either valueFromThese (show <<< toNumber) (table ^. _perPage)
  return
    [ absurd <$> output
    , H.div [ A.class_ VC.pagination ]
            [ prevButtons (page <= one)
            , pageField (valueFromThese (table ^. _page)) totalPages
            , nextButtons (page >= totalPages) totalPages
            , pageSize (isLeft (table ^. _perPage)) pageSizeValue
            ]
    ]
  where
  valueFromThese :: These String Int -> String
  valueFromThese = these id (show <<< toNumber) (\s _ -> s)

  prevButtons :: Boolean -> HTML e
  prevButtons enabled =
    H.div [ A.classes [B.btnGroup] ]
          [ H.button [ A.classes [B.btn, B.btnSm, B.btnDefault]
                     , A.disabled enabled
                     , E.onClick (\_ -> pure $ goPage one cell run)
                     ]
                     [ glyph B.glyphiconFastBackward ]
          , H.button [ A.classes [B.btn, B.btnSm, B.btnDefault]
                     , A.disabled enabled
                     , E.onClick (\_ -> pure $ stepPage (-one) cell run)
                     ]
                     [ glyph B.glyphiconStepBackward ]
          ]

  pageField :: String -> Int -> HTML e
  pageField pageValue totalPages =
    H.div [ A.classes [VC.pageInput] ]
          [ submittable [ H.text "Page"
                        , H.input [ A.classes [B.formControl, B.inputSm]
                                  , A.value pageValue
                                  , E.onInput (pure <<< inputPage cell)
                                  ]
                                  []
                        , H.text $ "of " ++ (show $ toNumber totalPages)
                        ]
          ]

  nextButtons :: Boolean -> Int -> HTML e
  nextButtons enabled lastPage =
    H.div [ A.classes [B.btnGroup] ]
          [ H.button [ A.classes [B.btn, B.btnSm, B.btnDefault]
                     , A.disabled enabled
                     , E.onClick (\_ -> pure $ stepPage one cell run)
                     ]
                     [ glyph B.glyphiconStepForward ]
          , H.button [ A.classes [B.btn, B.btnSm, B.btnDefault]
                     , A.disabled enabled
                     , E.onClick (\_ -> pure $ goPage lastPage cell run)
                     ]
                     [ glyph B.glyphiconFastForward ]
          ]

  pageSize :: Boolean -> String -> HTML e
  pageSize showCustom pageSizeValue =
    H.div [ A.classes [VC.pageSize] ]
          [ submittable $ [ H.text "Per page:" ]
                       ++ [ if showCustom
                            then H.input [ A.classes [B.formControl, B.inputSm]
                                         , A.value pageSizeValue
                                         , E.onInput (pure <<< inputPageSize cell)
                                         ]
                                         []
                            else H.select [ A.classes [B.formControl, B.inputSm]
                                          , E.onValueChanged (pure <<< changePageSize cell run)
                                          ]
                                          pageOptions
                          ]
          ]
    where
    pageOptions =
      let sizeValues = show <$> [10, 25, 50, 100]
      in (option <$> sizeValues)
         ++ [ H.option [ A.disabled true ] [ H.text $ fromChar $ fromCharCode 8212 ] ]
         ++ (if pageSizeValue `elemIndex` sizeValues == -1
             then [ H.option [ A.selected true ]
                             [ H.text pageSizeValue ]
                  ]
             else [])
         ++ [ H.option_ [ H.text "Custom" ] ]
    option value = H.option [ A.selected (value == pageSizeValue) ]
                            [ H.text value ]

  submittable :: [HTML e] -> HTML e
  submittable =
    H.form [ E.onSubmit (\_ -> E.preventDefault $> loadPage cell run) ]
