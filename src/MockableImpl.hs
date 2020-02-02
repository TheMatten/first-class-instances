{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}

module MockableImpl where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import FCI.Internal
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

instance c => GCaptureInst (K1 _1 (Improvised c)) where
  gcaptureInst = K1 inst


class (Generic d, GCaptureInst (Rep d)) => CaptureInst d
instance (Generic d, GCaptureInst (Rep d)) => CaptureInst d

captureInst :: CaptureInst d => d
captureInst = to gcaptureInst


coerceImprovisable :: Improvisable dict m a -> dict -> m a
coerceImprovisable = coerce



newtype Improvisable dict m a = Improvisable (ReaderT dict m a)
  deriving newtype (Functor, Applicative, Monad)

instance MonadTrans (Improvisable dict) where
  lift = Improvisable . lift


improvise :: dict -> Improvisable dict m a -> m a
improvise dict (Improvisable r) = runReaderT r dict

runImprovisable :: CaptureInst dict => Improvisable dict m a -> m a
runImprovisable (Improvisable r) = runReaderT r captureInst

