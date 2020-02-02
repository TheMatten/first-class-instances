{-# language TemplateHaskell #-}

module FCI.Control.Applicative (
    pattern Applicative, _Functor, _pure, (|<*>), _liftA2, (|*>), (|<*)
  , applyApplicative
  , liftA2Applicative
  , coerceApplicative
  , module M
  ) where

import Data.Coerce

import FCI.Data.Functor as M
import FCI.Internal.Definitions
import FCI.Internal.TH

-------------------------------------------------------------------------------
unsafeMkInst ''Applicative

-------------------------------------------------------------------------------
-- | Creates 'Applicative' instance from @apply@ ('<*>') definition.
applyApplicative :: (forall a. a -> f a)                    -- ^ 'pure'
                 -> (forall a b. f (a -> b) -> f a -> f b)  -- ^ ('<*>')
                 -> Inst (Applicative f)
applyApplicative _pure (|<*>) = Applicative{
    _Functor = fmapFunctor $ (|<*>) . _pure
  , _pure
  , (|<*>)
  , _liftA2  = \f fa fb -> _pure f |<*> fa |<*> fb
  , (|*>)    = \fa fb -> _pure (const id) |<*> fa |<*> fb
  , (|<*)    = \fa fb -> _pure const |<*> fa |<*> fb
  }

-------------------------------------------------------------------------------
-- | Creates 'Applicative' instance from 'liftA2' definition.
liftA2Applicative :: (forall a. a -> f a)
                  -- ^ 'pure'
                  -> (forall a b c. (a -> b -> c) -> f a -> f b -> f c)
                  -- ^ 'Control.Applicative.liftA2'
                  -> Inst (Applicative f)
liftA2Applicative _pure _liftA2 = Applicative{
    _Functor = fmapFunctor $ ($ _pure ()) . _liftA2 . const
  , _pure
  , (|<*>)   = _liftA2 ($)
  , _liftA2
  , (|*>)    = _liftA2 $ const id
  , (|<*)    = _liftA2 const
  }

-------------------------------------------------------------------------------
-- | Creates 'Applicative' instance for any type that can be "'coerce'd out".
coerceApplicative :: forall f. Newtype f => Inst (Applicative f)
coerceApplicative = Applicative{
    _Functor = coerceFunctor
  , _pure    = coerce
  -- TODO: can this be simplified?
  , (|<*>)   = coerce . (coerce :: f (a -> b) -> a -> b)
  , _liftA2  = coerce
  , (|*>)    = (coerce :: (a -> b -> b) -> f a -> f b -> f b) $ const id
  , (|<*)    = (coerce :: (a -> b -> a) -> f a -> f b -> f a) const
  }
