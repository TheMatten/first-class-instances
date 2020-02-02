{-# options_ghc -Wno-unused-imports #-}

-- | /First-class instances (FCI)/ - interface for explicit manipulation of
-- representations of constraints.
module FCI (
    Inst
  , mkInst
  , inst
  , (==>)
  , Newtype
  , Dict
  , module M
  ) where

import FCI.Internal.Definitions
import FCI.Internal.TH

import FCI.Control.Applicative as M
import FCI.Control.Monad       as M
import FCI.Data.Functor        as M
