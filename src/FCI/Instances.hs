{-# language TemplateHaskell #-}

-- | Instances that use TH from FCI.Internal (and so can't be defined there)
module FCI.Instances (
    pattern Functor, _fmap, (|<$)
  , fmapFunctor
  , pattern Applicative, super_Functor, _pure, (|<*>), _liftA2, (|*>), (|<*)
  , applyApplicative
  , pattern Monad, super_Applicative, _return, (|>>=), (|>>)
  , bindMonad, joinMonad
  ) where

import FCI.Internal

-------------------------------------------------------------------------------
unsafeMkInstRep ''Functor

fmapFunctor :: (forall a b. (a -> b) -> f a -> f b) -> Inst (Functor f)
fmapFunctor fmap_ = Functor fmap_ $ fmap_ . const

-------------------------------------------------------------------------------
unsafeMkInstRep ''Applicative

applyApplicative :: (forall a. a -> f a)
                 -> (forall a b. f (a -> b) -> f a -> f b)
                 -> Inst (Applicative f)
applyApplicative pure_ (<*>|) = Applicative
  do fmapFunctor ((<*>|) . pure_)
  do pure_
  do (<*>|)
  do \f x y -> pure_ f <*>| x <*>| y
  do \x y -> pure_ (const id) <*>| x <*>| y
  do \x y -> pure_ const <*>| x <*>| y

-------------------------------------------------------------------------------
unsafeMkInstRep ''Monad

bindMonad :: (forall a. a -> m a)
          -> (forall a b. m a -> (a -> m b) -> m b)
          -> Inst (Monad m)
bindMonad return_ (>>=|) = Monad
  do applyApplicative return_ \f x ->
       f >>=| \f' -> x >>=| \x' -> return_ $ f' x'
  do (>>=|)
  do \x y -> x >>=| const y
  do return_
  do error

joinMonad :: Inst (Functor m)
          -> (forall a. a -> m a)
          -> (forall a. m (m a) -> m a)
          -> Inst (Monad m)
joinMonad Functor{ _fmap } return_ join_ = Monad
  do applyApplicative return_ \f x -> join_ $ _fmap (\f' -> _fmap f' x) f
  do \x f -> join_ $ _fmap f x
  do \x y -> join_ $ _fmap (const y) x
  do return_
  do error