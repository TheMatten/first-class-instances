{-# language TemplateHaskell #-}

module FCI.Data.Functor (
    Dict (..)
  , fmapFunctor
  ) where

import FCI.Internal

-------------------------------------------------------------------------------
unsafeMkInst ''Functor

-------------------------------------------------------------------------------
-- | Creates 'Functor' instance from mapping function.
fmapFunctor :: (forall a b. (a -> b) -> f a -> f b) -> Inst (Functor f)
fmapFunctor _fmap = Functor {
    _fmap
  , (|<$) = _fmap . const
  }