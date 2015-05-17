module Model.Notebook.Cell.Common where

import Optic.Core (Lens(), lens)

_input :: forall a b r. Lens {input :: a | r} {input :: b | r} a b 
_input = lens _.input _{input = _} 

_table :: forall a b r. Lens {table :: a | r} {table :: b | r} a b 
_table = lens _.table _{table = _}
