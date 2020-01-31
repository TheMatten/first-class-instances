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


class MonadFoo m where
  foo :: m Int
makeMockable ''MonadFoo


class MonadBar m where
  bar :: Int -> m a -> m a
  baz :: m Int
makeMockable ''MonadBar

class Monad m => MonadMask m where
  mask :: ((m Int -> m a) -> (m a -> Int) -> m Int) -> m a
makeMockable ''MonadMask


data AppDicts m = AppDicts
  { _appDictsMonadFoo :: Inst (MonadFoo m)
  , _appDictsMonadBar :: Inst (MonadBar m)
  , _appDictsMonadMask :: Inst (MonadMask m)
  } deriving stock Generic
makeFields ''AppDicts

iAmMocked :: Monad m => Mockable AppDicts m Int
iAmMocked = bar 5 $ do
  x <- foo
  y <- baz
  pure $ x + y

iAmNotMocked :: (MonadFoo m, MonadBar m, MonadMask m) => m Int
iAmNotMocked = runUnmocked iAmMocked


