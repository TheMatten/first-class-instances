{-# language NoMonoLocalBinds, TemplateHaskell #-}

-- | TH for dictionary representation generation. This module is internal and
-- provides no guarantees about stability and safety of it's interface.
module FCI.Internal.TH (
    mkInst
  , unsafeMkInst
  , getClassDictInfo
  , dictInst
  ) where

import Language.Haskell.TH.Syntax

import           Control.Monad       (when, unless)
import           Control.Monad.ST    (runST)
import           Data.Char           (isAlpha)
import qualified Data.Kind as K
import           Data.List           (foldl1')
import qualified Data.Map.Strict as M
import           Data.Maybe          (mapMaybe)
import           Data.STRef          (newSTRef, readSTRef, modifySTRef)
import           Language.Haskell.TH (thisModule)

import FCI.Internal.Types (Inst, Dict)

-------------------------------------------------------------------------------
-- | Creates first class instance representation from based on class. Generated
-- representation is record of class members following simple format:
--
-- * name of record constructor is name of original class
--
-- * superclass constraints are transformed into fields containing their
--   representation, names of fields are generated this way:
--
--     * Prefix names ('Show', 'Applicative') are prefixed with @_@
--     * Operators (('~')) are prefixed with @||@
--     * Tuples are converted into prefix names @_Tuple@
--     * Additional occurencies of same prefix name get postfix index starting
--       from 1
--     * Additional occurencies of same operator are postfixed with increasing
--     number of @|@s
--
-- * methods get their own fields, names of fields are names of methods
--   prefixed with @_@
--
-- To avoid possibly breaking assumptions author of class may have made about
-- it's instances, you can only create representation for class in current
-- module.
mkInst :: Name -> Q [Dec]
mkInst name = checkSafeInst name *> unsafeMkInst name
-- TODO: example

-------------------------------------------------------------------------------
-- | Checks that it is save to create 'Inst' instance for given name.
checkSafeInst :: Name -> Q ()
checkSafeInst name = do
  -- TODO: we may want to support them - plus, what already works with QC?
  isExtEnabled QuantifiedConstraints >>= flip when do
    fail "'QuantifiedConstraints' are not supported yet"
  Module _ (ModName this_module) <- thisModule
  unless (nameModule name == Just this_module) $ fail
    $  '\'' : nameBase name ++ "' is not declared in current module '"
    ++ this_module ++ "'"

-------------------------------------------------------------------------------
-- | Version of 'mkInst' without any checks. You shouldn't use it unless you're
-- working on this library.
unsafeMkInst :: Name -> Q [Dec]
unsafeMkInst = fmap dictInst . getClassDictInfo

-------------------------------------------------------------------------------
-- | Constructs info about class dictionary represenation being created.
getClassDictInfo :: Name -> Q ClassDictInfo
getClassDictInfo className = reify className >>= \case
  ClassI (ClassD constraints _ args _ methods) _ -> do
    dictConName <- dictConFromClassName className
    pure CDI{
        className
      , dictTyArg   = foldl1' AppT $ ConT className : map bndrToType args
      , dictConName
      , dictFields  = superFieldsFromCxt constraints
                   ++ mapMaybe methodFieldFromDec methods
      }
  _ -> fail $ '\'' : nameBase className ++ "' is not a class"

-------------------------------------------------------------------------------
-- | Creates name of dictionary representation data constructor from name of
-- class. Name is generated this way:
--
-- * Prefix names ('Show', 'Applicative') are kept as-is
-- * Operators (('~')) are prefixed with colon @:@
-- * Tuples are not supported (they have custom 'Inst' instances)
dictConFromClassName :: Name -> Q Name
dictConFromClassName (nameBase -> name@(c : _)) = mkName <$> if
  | isAlpha_ c -> pure name
  | c == '('   -> fail $ "Attempt to use restricted class '" ++ name ++ "'"
  | otherwise  -> pure $ ':':name
dictConFromClassName _ = error "dictConFromClassName: empty 'Name'"

-------------------------------------------------------------------------------
-- | Creates class dictionary representation fields from constraints that carry
-- runtime proof, preserving order.
superFieldsFromCxt :: [Pred] -> [ClassDictField]
superFieldsFromCxt constraints = runST do
  counts <- newSTRef M.empty
  sequence $ mapMaybe (fmap . mkSuperField counts <*> appHeadName) constraints
 where
  mkSuperField counts c n = do
    count <- maybe 0 id . M.lookup n <$> readSTRef counts
    modifySTRef counts $ M.alter (maybe (Just 1) $ Just . (+1)) n
    pure CDF{
        fieldName   = fieldFromClassName n count
      , fieldSource = Superclass
      , origName    = n
      , origType    = c
      }

-------------------------------------------------------------------------------
-- | Creates name of field holding superclass instance from name of class. Name
-- is generated this way:
--
-- * Prefix names ('Show', 'Applicative') are prefixed with @_@
-- * Operators (('~')) are prefixed with @||@
-- * Tuples are converted into prefix names "_Tuple"
--
-- If there are multiple constraints with same name:
--
-- * Prefix names and names of tuples get numeric suffixes in order
-- * Operators are suffixed with increasing number of @|@
fieldFromClassName :: Name -> Int -> Name
fieldFromClassName (nameBase -> name@(c:_)) count = mkName if
  | isAlpha_ c -> "_"  ++ name     ++ index
  | c == '('   ->         "_Tuple" ++ index
  | otherwise  -> "||" ++ name     ++ replicate count '|'
  where
    index = if count == 0 then "" else show count
fieldFromClassName _ _ = error "fieldFromClassName: empty 'Name'"

-------------------------------------------------------------------------------
-- | Converts type variable binder to type.
bndrToType :: TyVarBndr -> Type
bndrToType = \case
  PlainTV n    -> VarT n
  KindedTV n k -> VarT n `SigT` k

-------------------------------------------------------------------------------
-- | Extracts name of head of type application or returns 'Nothing'.
appHeadName :: Type -> Maybe Name
appHeadName = \case
  ForallT _ _ t    -> appHeadName t
  AppT t _         -> appHeadName t
  SigT t _         -> appHeadName t
  VarT n           -> Just n
  ConT n           -> Just n
  PromotedT n      -> Just n
  InfixT _ n _     -> Just n
  UInfixT _ n _    -> Just n
  ParensT t        -> appHeadName t
  TupleT i         -> prod "("  ',' (i - 1)  ")"
  UnboxedTupleT i  -> prod "(#" ',' (i - 1) "#)"
  UnboxedSumT i    -> prod "(#" '|' (i + 1) "#)"
  ArrowT           -> Just ''(->)
  EqualityT        -> Just ''(~)
  ListT            -> Just ''[]
  PromotedTupleT i -> prod "(" ',' (i - 1) ")"
  PromotedNilT     -> Just '[]
  PromotedConsT    -> Just '(:)
  StarT            -> Just ''K.Type
  ConstraintT      -> Just ''K.Constraint
  LitT{}           -> Nothing
  WildCardT        -> Nothing
 where
  prod l d i r  = Just $ mkName if
    | i <= 0    -> l                  ++ r
    | otherwise -> l ++ replicate i d ++ r

-------------------------------------------------------------------------------
-- | Creates class dictionary representation field from class member of returns
-- 'Nothing'.
methodFieldFromDec :: Dec -> Maybe ClassDictField
methodFieldFromDec = \case
  SigD n (ForallT _ _ t) -> Just CDF{
      fieldName   = fieldFromMethodName n
    , fieldSource = Method
    , origName    = n
    , origType    = t
    }
  _ -> Nothing

-------------------------------------------------------------------------------
-- | Creates name of field holding method implementation from method name. Name
-- is generated this way:
--
-- * Prefix names ('show', 'pure') are prefixed with @_@
-- * Operators (('<*>'), ('>>=')) are prefixed with @|@
fieldFromMethodName :: Name -> Name
fieldFromMethodName (nameBase -> name@(c:_)) = mkName if
  | isAlpha_ c -> '_':name
  | otherwise  -> '|':name
fieldFromMethodName _ = error "fieldFromMethodName: empty 'Name'"

-------------------------------------------------------------------------------
-- | Creates 'Dict' instance from info about class dictionary representation.
dictInst :: ClassDictInfo -> [Dec]
dictInst cdi = [
    TySynInstD ''Inst $
      TySynEqn [dictTyArg cdi] $ ConT ''Dict `AppT` dictTyArg cdi
  , case classDictToRecField <$> dictFields cdi of
      []      -> mk DataInstD    [NormalC (dictConName cdi) []     ]
      [field] -> mk NewtypeInstD (RecC    (dictConName cdi) [field])
      fields  -> mk DataInstD    [RecC    (dictConName cdi) fields ]
  ]
 where
  mk con fields = con [] ''Dict [dictTyArg cdi] Nothing fields []

-------------------------------------------------------------------------------
-- | Converts info about class dictionary representation field to record field.
classDictToRecField :: ClassDictField -> VarBangType
classDictToRecField cdf = (
    fieldName cdf
  , Bang NoSourceUnpackedness NoSourceStrictness
  , (case fieldSource cdf of
      Superclass -> AppT $ ConT ''Inst
      Method     -> id
    ) $ origType cdf
  )

-------------------------------------------------------------------------------
-- | Info about class dictionary used by 'mkInst'.
data ClassDictInfo = CDI{
    className   :: Name
  , dictTyArg   :: Pred
  , dictConName :: Name
  , dictFields  :: [ClassDictField]
  } deriving Show

-------------------------------------------------------------------------------
-- | Info about field in class dictionary used by 'mkInst'
data ClassDictField = CDF{
    fieldName   :: Name
  , fieldSource :: ClassDictFieldSource
  , origName    :: Name
  , origType    :: Type
  } deriving Show

-------------------------------------------------------------------------------
-- | Source of field in class dictionary.
data ClassDictFieldSource = Superclass | Method deriving Show

-------------------------------------------------------------------------------
-- | Checks if character is part of alphabet or underscore.
isAlpha_ :: Char -> Bool
isAlpha_ c = isAlpha c || c == '_'
