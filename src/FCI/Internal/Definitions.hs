{-# language MagicHash #-}

-- | Core definitions. This module is internal and provides no guarantees about
-- stability and safety of it's interface.
module FCI.Internal.Definitions (
    Inst
  , Dict
  , inst
  , (==>)
  , Newtype
  ) where

import Data.Coerce (Coercible)
import GHC.Exts    (unsafeCoerce#)

import qualified FCI.Internal.Types as Internal (Inst, Dict)

-------------------------------------------------------------------------------
infixr 0 ==>
infixr 1 :=>

-------------------------------------------------------------------------------
-- | Type that maps constraint to it's representation. You can get hold of
-- representation of some special constraints and classes that use
-- 'FCI.mkInst'.
--
-- For example:
--
-- @
-- class Bar a => Foo a where
--   baz :: a
--   qux :: a -> b -> [(a, b)]
--
-- mkInst 'Foo
-- @
--
-- creates datatype instance:
--
-- @
-- type instance Inst (Foo a) = Dict (Foo a)
-- data instance Dict (Foo a) = Foo{
--     _Bar :: Inst (Bar a)
--   , baz  :: a
--   , qux  :: forall b. a -> b -> [(a, b)]
--   }
-- @
--
-- You can get hold of representation of global instance using 'FCI.inst'. You
-- are free to modify and read it and you can use ('FCI.==>') to apply it as
-- constraint in context of some subexpression. See 'FCI.mkInst' for more info
-- about format of generated representation.
type Inst c = Internal.Inst c

-------------------------------------------------------------------------------
-- | Type of representation of class instance. You can get instance for your
-- class using 'FCI.mkInst' and access value of global instance using
-- 'FCI.inst'. Prefer 'FCI.Inst' in signatures when working with constraint
-- representations.
type Dict = Internal.Dict

-------------------------------------------------------------------------------
-- | /Reflects/ constraint into correspoding representation - can be used to
-- access normal class instances from the environment. This function is meant
-- to be used with @TypeApplications@ when it's usage is ambiguous.
--
-- TODO: example
inst :: forall c. c => Inst c
inst = case unsafeCoerce# id :: c :=> Inst c of Wants d -> d

-------------------------------------------------------------------------------
-- | /Reifies/ first class instance into constraint in context of supplied
-- continuation.
--
-- For example:
--
-- >>> newtype Foo a = Foo a deriving Show
-- >>> coerceFunctor @Foo ==> (+1) <$> Foo 1
-- Foo 2
(==>) :: forall c r. Inst c -> (c => r) -> r
d ==> x = unsafeCoerce# (Wants @c @r x) d

-------------------------------------------------------------------------------
-- | Wrapper for value @r@ requiring constraint @c@. Used by 'inst' and ('==>')
-- to satisfy typechecker.
newtype c :=> r where
  Wants :: (c => r) -> c :=> r

-------------------------------------------------------------------------------
-- | Allows to 'Data.Coerce.coerce' type back and forth between it's argument
-- when safe.
type Newtype f = forall a. (Coercible a (f a), Coercible (f a) a)
