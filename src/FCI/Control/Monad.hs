{-# language TemplateHaskell #-}

module FCI.Control.Monad (
    pattern Monad, _Applicative, (|>>=), (|>>), _return, _fail
  , bindMonad
  , joinMonad
  , coerceMonad
  , module M
  ) where

import Data.Coerce
import Data.Function

import FCI.Control.Applicative as M
import FCI.Internal.Definitions
import FCI.Internal.TH

-------------------------------------------------------------------------------
unsafeMkInst ''Monad

-------------------------------------------------------------------------------
-- | Creates 'Monad' instance from @bind@ ('>>=') definition.
bindMonad :: (forall a. a -> m a)                    -- ^ 'return'
          -> (forall a b. m a -> (a -> m b) -> m b)  -- ^ ('>>=')
          -> Inst (Monad m)
bindMonad _return (|>>=) = Monad{
    _Applicative = applyApplicative _return \mf ma ->
                     mf |>>= \f -> ma |>>= \a -> _return $ f a
  , (|>>=)
  , (|>>)        = \ma -> (ma |>>=) . const
  , _return
  , _fail        = error
  }

-------------------------------------------------------------------------------
-- | Creates 'Monad' instance from @join@ definition.
joinMonad :: (forall a b. (a -> b) -> m a -> m b)  -- ^ 'fmap'
          -> (forall a. a -> m a)                  -- ^ 'return'
          -> (forall a. m (m a) -> m a)            -- ^ 'Control.Monad.join'
          -> Inst (Monad m)
joinMonad _fmap _return _join = Monad{
    _Applicative = applyApplicative _return \mf ma ->
                     _join $ _fmap (`_fmap` ma) mf
  , (|>>=)       = \ma -> _join . flip _fmap ma
  , (|>>)        = \ma -> _join . flip _fmap ma . const
  , _return
  , _fail        = error
  }

-------------------------------------------------------------------------------
-- | Creates 'Monad' instance for any type that can be "'coerce'd out".
coerceMonad :: forall m. Newtype m => Inst (Monad m)
coerceMonad = Monad{
    _Applicative = coerceApplicative
  , (|>>=)       = (coerce :: (a -> (a -> b) -> b) -> m a -> (a -> m b) -> m b)
                     (&)
  , (|>>)        = (coerce :: (a -> b -> b) -> m a -> m b -> m b) $ const id
  , _return      = coerce
  , _fail        = error
  }
