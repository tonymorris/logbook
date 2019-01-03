module Util where

import Control.Lens

---- does this exist somewhere?

unproduct ::
  Iso ((), a) ((), b) a b
unproduct =
  iso
    (\((), a) -> a)
    (\a -> ((), a))

