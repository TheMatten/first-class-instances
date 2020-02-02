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
import FCI


class Monad m => MonadFoo m where
  foo :: m Int
makeMockable ''MonadFoo


class Monad m => MonadBar m where
  bar :: Int -> m a -> m a
  baz :: m Int
makeMockable ''MonadBar

class Monad m => MonadMask m where
  mask :: ((m Int -> m a) -> (m a -> Int) -> m Int) -> m a
makeMockable ''MonadMask


data AppDicts m = AppDicts
  { _appDictsMonadFoo :: Inst (MonadFoo m)
  , _appDictsMonadBar :: Inst (MonadBar m)
  } deriving stock Generic
makeFields ''AppDicts


myBusinessLogic :: (MonadFoo m, MonadBar m) => m Int
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
        (MonadFoo' (pure 5))
        (MonadBar' (const id) (pure 7))

pattern MonadFoo' :: Monad m => m Int -> Dict (MonadFoo m)
pattern MonadFoo' foo <- MonadFoo _ foo
  where
    MonadFoo' foo = MonadFoo inst foo

pattern MonadBar' :: Monad m => (forall a. Int -> m a -> m a) -> m Int -> Dict (MonadBar m)
pattern MonadBar' bar baz <- MonadBar _ bar baz
  where
    MonadBar' bar baz = MonadBar inst bar baz

myUnmockedBusinessLogic :: (MonadFoo m, MonadBar m) => m Int
myUnmockedBusinessLogic = runUnmocked myMockedBusinessLogic

zop :: IO ()
zop = runApp $ pure ()

