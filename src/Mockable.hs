{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}

module Mockable
  ( Improvisable ()
  , improvise
  -- , runImprovisable
  , Improvised
  -- , CaptureInst ()
  , makeImprovised
  , makeImprovCollection
  ) where

import FCI.Internal
import MockableImpl
import MyTH

