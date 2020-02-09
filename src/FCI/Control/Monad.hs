{-# language CPP, TemplateHaskell #-}

module FCI.Control.Monad (
    pattern Monad, _Applicative, (|>>=), (|>>), _return
#if !MIN_VERSION_GLASGOW_HASKELL(8,8,0,0)
  , _fail
#endif
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
#if !MIN_VERSION_GLASGOW_HASKELL(8,8,0,0)
  , _fail        = error
#endif
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
#if !MIN_VERSION_GLASGOW_HASKELL(8,8,0,0)
  , _fail        = error
#endif
  }

-------------------------------------------------------------------------------
-- | Creates 'Monad' instance for any type that can be "'coerce'd out".
coerceMonad :: forall m
             . ( forall a b. Coercible (m (a -> b)) (m a -> m b)
               , forall a  . Coercible a            (m a)
               )
            => Inst (Monad m)
coerceMonad = Monad{
    _Applicative = coerceApplicative
  , (|>>=)       = (coerce :: (a -> (a -> b) -> b) -> m a -> (a -> m b) -> m b)
                     (&)
  , (|>>)        = (coerce :: (a -> b -> b) -> m a -> m b -> m b) $ const id
  , _return      = coerce
#if !MIN_VERSION_GLASGOW_HASKELL(8,8,0,0)
  , _fail        = error
#endif
  }
