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

import Control.Lens
import           MyTH
import Mockable
import App


class Monad m => MonadFoo s m where
  foo :: m s
makeMockable ''MonadFoo


class Monad m => MonadBar m where
  bar :: Int -> m a -> m a
  baz :: m Int
makeMockable ''MonadBar

class Monad m => MonadMask m where
  mask :: ((m Int -> m a) -> (m a -> Int) -> m Int) -> m a
makeMockable ''MonadMask

-- doesn't work for MULTIPLE OF SAME SAME

data AppDicts m = AppDicts
  { _appDictsMonadFoo :: Inst (MonadFoo Int m)
  , _appDictsMonadBar :: Inst (MonadBar m)
  } deriving stock Generic
makeFields ''AppDicts


myBusinessLogic :: (MonadFoo Int m, MonadBar m) => m Int
myBusinessLogic =
  bar 5 $ do
    x <- foo
    y <- baz
    pure $ x + y

myMockedBusinessLogic :: Monad m => Mockable AppDicts m Int
myMockedBusinessLogic = myBusinessLogic

tested :: Monad m => m Int
tested = runMocked mocks myMockedBusinessLogic
  where
    mocks =
      AppDicts
        (MonadFoo (pure 5))
        (MonadBar (const id) (pure 7))

myUnmockedBusinessLogic :: (MonadFoo Int m, MonadBar m) => m Int
myUnmockedBusinessLogic = runUnmocked myMockedBusinessLogic

-- zop :: IO ()
-- zop = runApp $ pure ()

