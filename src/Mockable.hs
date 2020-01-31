{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}

module Mockable
  ( module Mockable
  , Inst
  ) where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import FCI
import GHC.Generics
import Data.Coerce


class GCaptureInst i where
  gcaptureInst :: i x

instance GCaptureInst f => GCaptureInst (M1 _1 _2 f) where
  gcaptureInst = M1 gcaptureInst

instance GCaptureInst U1 where
  gcaptureInst = U1

instance (GCaptureInst f, GCaptureInst g) => GCaptureInst (f :*: g) where
  gcaptureInst = gcaptureInst :*: gcaptureInst

instance (c, Inst c ~ Dict c) => GCaptureInst (K1 _1 (Dict c)) where
  gcaptureInst = K1 inst


class (Generic d, GCaptureInst (Rep d)) => CaptureInst d
instance (Generic d, GCaptureInst (Rep d)) => CaptureInst d

captureInst :: CaptureInst d => d
captureInst = to gcaptureInst


coerceMockable :: Mockable dict m a -> dict m -> m a
coerceMockable = coerce



newtype Mockable dict m a = Mockable (ReaderT (dict m) m a)
  deriving newtype (Functor, Applicative, Monad)

instance MonadTrans (Mockable dict) where
  lift = Mockable . lift


runMocked :: dict m -> Mockable dict m a -> m a
runMocked dict (Mockable r) = runReaderT r dict

runUnmocked :: CaptureInst (dict m) => Mockable dict m a -> m a
runUnmocked (Mockable r) = runReaderT r captureInst

