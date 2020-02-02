{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}

module Mockable
  ( Mockable ()
  , runMocked
  , runUnmocked
  , Inst
  , CaptureInst ()
  ) where

import FCI
import MockableImpl

