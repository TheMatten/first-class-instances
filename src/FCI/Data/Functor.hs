{-# language TemplateHaskell #-}

module FCI.Data.Functor (
    pattern Functor, _fmap, (|<$)
  , fmapFunctor
  , coerceFunctor
  ) where

import Data.Coerce

import FCI.Internal.Definitions
import FCI.Internal.TH

-------------------------------------------------------------------------------
unsafeMkInst ''Functor

-------------------------------------------------------------------------------
-- | Creates 'Functor' instance from mapping function.
fmapFunctor :: (forall a b. (a -> b) -> f a -> f b) -> Inst (Functor f)
fmapFunctor _fmap = Functor{
    _fmap
  , (|<$) = _fmap . const
  }

-------------------------------------------------------------------------------
-- | Creates 'Functor' instance for any type that can be "'coerce'd out".
coerceFunctor :: forall f. Newtype f => Inst (Functor f)
coerceFunctor = Functor{
    _fmap = coerce
  , (|<$) = (coerce :: (a -> b -> a) -> a -> f b -> f a) const
  }
