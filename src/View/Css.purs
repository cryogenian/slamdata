module View.Css where

import Halogen.HTML.Attributes (className, ClassName())

searchInput :: ClassName
searchInput = className "search-input"

results :: ClassName
results = className "results"
