{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE NoPolyKinds                #-}
{-# LANGUAGE NoQuantifiedConstraints    #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -ddump-splices          #-}

module Ok where

import MyTH
import Mockable
import App


class Monad m => MonadFoo s m | m -> s where
  foo :: m s
makeMockable ''MonadFoo


class Monad m => MonadBar m where
  bar :: Int -> m a -> m a
  baz :: m Int
makeMockable ''MonadBar

class Monad m => MonadCont m where
  callCC :: ((a -> m b) -> m a) -> m a
makeMockable ''MonadCont


data AppDicts s m =
  AppDicts
    (Dict (MonadFoo s m))
    (Dict (MonadBar m))
  deriving stock Generic

mkMockableDict ''AppDicts


myBusinessLogic :: (MonadFoo Int m, MonadBar m) => m Int
myBusinessLogic =
  bar 5 $ do
    x <- foo
    y <- baz
    pure $ x + y

tested :: Monad m => m Int
tested = runMocked mocks myBusinessLogic
  where
    mocks =
      AppDicts
        (MonadFoo (pure 5))
        (MonadBar (const id) (pure 7))

