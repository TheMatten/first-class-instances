{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE NoPolyKinds                #-}
{-# LANGUAGE NoQuantifiedConstraints    #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -ddump-splices          #-}

module Ok where

import Mockable
import Control.Monad.Trans.Writer


------------------------------------------------------------------------------
-- | MonadFoo lets you foo
class Monad m => MonadFoo s m | m -> s where
  foo :: m s
  faz :: s -> m ()
makeImprovised ''MonadFoo


data Action s = Faz s
  deriving Show

test :: Improvised (MonadFoo Int (Writer [Action Int]))
test =
  MonadFoo
    { _faz = \s -> do
        tell [Faz s]
    , _foo = pure 5
    }

ok :: [Action Int]
ok = snd . runWriter . improvise test $ do
  s <- foo
  faz s
  faz 10


------------------------------------------------------------------------------
-- | MonadBar is a pretty cool guy
class Monad m => MonadBar m where
  bar :: Int -> m a -> m a
  baz :: m Int
makeImprovised ''MonadBar


------------------------------------------------------------------------------
-- | The continuation monad
class Monad m => MonadCont m where
  callCC :: ((a -> m b) -> m a) -> m a
makeImprovised ''MonadCont


------------------------------------------------------------------------------
-- | A little dict for doing things
data AppDicts s m =
  AppDicts
    (Improvised (MonadFoo s m))
    (Improvised (MonadBar m))
makeImprovCollection ''AppDicts


------------------------------------------------------------------------------
-- | ok business
myBusinessLogic :: (MonadFoo Int m, MonadBar m) => m Int
myBusinessLogic =
  bar 5 $ do
    x <- foo
    y <- baz
    pure $ x + y

------------------------------------------------------------------------------
-- | mocked out baby
tested :: Monad m => m Int
tested = improvise mocks myBusinessLogic
  where
    mocks =
      AppDicts
        (MonadFoo (pure 5) (const $ pure ()))
        (MonadBar (const id) (pure 7))

