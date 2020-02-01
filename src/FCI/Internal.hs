{-# options_ghc -Wno-redundant-constraints #-}

module FCI.Internal (
    module M
  , Newtype
  ) where

import Data.Coerce (Coercible)

import FCI.Internal.Plugin      as M
import FCI.Internal.Definitions as M
import FCI.Internal.TH          as M

-------------------------------------------------------------------------------
-- | Allows to 'coerce' type back and forth between it's argument when safe.
type Newtype f = forall a. (Coercible a (f a), Coercible (f a) a)