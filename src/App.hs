{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App (module App) where

newtype App a = App (IO a)
  deriving newtype (Functor, Applicative, Monad)

runApp :: App a -> IO a
runApp (App m) = m

{-# WARNING App
      "The App type must never be used concretely."
#-}


