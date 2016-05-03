module SlamData.Notebook.Card.DownloadOptions.Component.Query where

import SlamData.Prelude
import SlamData.Notebook.Card.Common.EvalQuery (CardEvalQuery)
import SlamData.Download.Model (CSVOptions, JSONOptions, OutputType)

data Query a
  = SetOutput OutputType a
  | ModifyCSVOpts (CSVOptions → CSVOptions) a
  | ModifyJSONOpts (JSONOptions → JSONOptions) a
  | ToggleCompress a

type QueryP = CardEvalQuery ⨁ Query
