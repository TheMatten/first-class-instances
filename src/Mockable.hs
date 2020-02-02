{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}

module Mockable
  ( Mockable ()
  , runMocked
  , runUnmocked
  , Dict
  , CaptureInst ()
  , makeMockable
  , mkMockableDict
  ) where

import FCI
import MockableImpl
import MyTH

