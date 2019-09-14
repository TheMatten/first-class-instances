module FCI (
    Inst
  , Dict
  , module Instances
  ) where

import qualified FCI.Internal  as I
import           FCI.Instances as Instances

type Inst c = I.Inst c
type Dict = I.Dict