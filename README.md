# `first-class-instances`

**(Work in progress!)**

Remember when you had some neat way of building instance out of another one,
when some type had multiple interesting instances of the same class, or when it
was possible to elegantly derive some instance for your type on paper, but
compiler just wouldn't let you to use these things because of overlapping and stuff?
Well, now you can!:

```hs
{-# language TemplateHaskell, TypeFamilies, NoQuantifiedConstraints #-}

module Main where

import Prelude hiding (read)

import FCI

class Monad m => Teletype m where
  read  :: m String
  write :: String -> m ()

mkInst ''Teletype

hello :: Teletype m => m ()
hello = do
  write "What's your name?: "
  name <- read
  write $ "Hi " ++ name ++ "!\n"

main :: IO ()
main = Teletype inst getLine putStr ==> hello
```

`first-class-instances` allow you to treat typeclass instances as what they
actually are - ordinary values. And Magic Inside™ turns them back and forth
between ordinary constraints when needed. Build instances from the others,
locally from available functions or even dynamically at runtime! Or derive them
using ordinary Haskell code and then promote into global ones using TH (TODO).

This library is still experimental, so please do not use it for serious things,
but I welcome you to try it out and give some feedback!

# Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md)
