module FCI.Internal (
    module M
  , inst
  , (==>)
  , Newtype
  ) where

import Data.Coerce
import Unsafe.Coerce

import FCI.Internal.Types as M
import FCI.Internal.TH    as M

-------------------------------------------------------------------------------
infixr 0 ==>
infixr 1 :=>

-------------------------------------------------------------------------------
-- | /Reflects/ constraint into correspoding representation - can be used to
-- access normal class instances from the environment. This function is meant
-- to be used with @TypeApplications@ when it's usage is ambiguous.
--
-- TODO: example
inst :: forall c. c => Improvised c
inst = case unsafeCoerce id :: c :=> Improvised c of Wants d -> d

-------------------------------------------------------------------------------
-- | /Reifies/ first class instance into constraint in context of supplied
-- continuation.
--
-- TODO: example
(==>) :: forall c r. Improvised c -> (c => r) -> r
d ==> r = unsafeCoerce (Wants @c @r r) d

-------------------------------------------------------------------------------
-- | Type of computation @a@ requiring constraint @c@ - used internally when
-- transforming between such value and it's actual representation as a
-- function.
newtype c :=> a where
  Wants :: (c => a) -> c :=> a

-------------------------------------------------------------------------------
-- | Allows to 'coerce' type back and forth between it's argument when safe.
type Newtype f = forall a. (Coercible a (f a), Coercible (f a) a)
