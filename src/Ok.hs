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
import           Mockable
import           MyTH
import FCI
import Control.Monad.Trans.Reader


-- class MonadFoo m where
--   foo :: m Int
-- makeMockable ''MonadFoo


-- class MonadBar m where
--   bar :: Int -> m a -> m a
--   baz :: m Int
-- makeMockable ''MonadBar

class Monad m => MonadState s m | m -> s where
  get :: m s
  put :: s -> m ()
  state :: (s -> (s, a)) -> m a
makeMockable ''MonadState





-- makeMockable ''MonadState


-- data AppDicts m = AppDicts
--   { _appDictsMonadFoo :: Inst (MonadFoo m)
--   , _appDictsMonadBar :: Inst (MonadBar m)
--   } deriving stock Generic
-- makeFields ''AppDicts

-- iAmMocked :: Monad m => Mockable AppDicts m Int
-- iAmMocked = bar 5 $ do
--   x <- foo
--   y <- baz
--   pure $ x + y

-- iAmNotMocked :: (Monad m, MonadFoo m, MonadBar m) => m Int
-- iAmNotMocked = runUnmocked iAmMocked


