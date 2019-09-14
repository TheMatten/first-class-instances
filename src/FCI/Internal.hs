{-# language TemplateHaskell #-}

module FCI.Internal (
    Inst
  , Dict
  , inst
  , (==>)
  , unsafeMkInstRep
  , (:=:)
  ) where

import           Data.Char
import           Data.Function
import           Data.Functor
import           Data.Kind
import           Data.List
import           Data.Maybe
import           Language.Haskell.TH        hiding (Type)
import qualified Language.Haskell.TH as TH
import           Language.Haskell.TH.Syntax hiding (Type)
import           Unsafe.Coerce

-------------------------------------------------------------------------------
infixr 0 ==>
infixr 1 :=>

-------------------------------------------------------------------------------
type family Inst (c :: Constraint) = (t :: Type) | t -> c

-------------------------------------------------------------------------------
data family Dict :: Constraint -> Type

-------------------------------------------------------------------------------
inst :: forall c. c => Inst c
inst = case unsafeCoerce id :: c :=> Inst c of Wants c -> c

-------------------------------------------------------------------------------
(==>) :: forall c r. Inst c -> (c => r) -> r
c ==> a = unsafeCoerce (Wants @c @r a) c

-------------------------------------------------------------------------------
unsafeMkInstRep :: Name -> DecsQ
unsafeMkInstRep name = do
  ClassI (ClassD constraints _ args _ decs) _ <- reify name
  let constraint = foldl1' AppT $ ConT name : map bndrToType args
  superFields' <- superFields constraints

  pure [
      TySynInstD ''Inst $ TySynEqn [constraint] $
        ConT ''Dict `AppT` constraint
    , dictInstD constraint (dictConName name) $
        superFields' ++ methodFields decs
    ]
 where
  dictConName (nameBase -> name@(c:_)) =
    mkName $ (if isAlpha_ c then id else (':':)) name

-------------------------------------------------------------------------------
dictInstD :: TH.Type -> Name -> [VarBangType] -> Dec
dictInstD constraint conName = \case
  []      ->
    DataInstD [] ''Dict [constraint] Nothing [NormalC conName []] []
  [field] ->
    NewtypeInstD [] ''Dict [constraint] Nothing (RecC conName [field]) []
  fields  ->
    DataInstD [] ''Dict [constraint] Nothing [RecC conName fields] []

-------------------------------------------------------------------------------
superFields :: [Pred] -> Q [VarBangType]
superFields = fmap catMaybes . traverse \constraint ->
  appHead constraint & \case
    ConT name -> reify name >>= \case
      ClassI{}  -> pure $ Just $ superField (nameBase name) constraint
      FamilyI{} -> pure Nothing
    VarT name -> pure $ Just $ superField (nameBase name) constraint
    TupleT i  -> pure $ Just $ superField ("T" ++ show i) constraint
    -- TODO: fix name conflicts
    EqualityT -> pure $ Just $ superField "Equality" constraint
 where
  superField name@(c:_) constraint = (
      mkName $ (if isAlpha_ c then "super_" else "/\\") ++ name
    , Bang NoSourceUnpackedness NoSourceStrictness
    , ConT ''Inst `AppT` constraint
    )

-------------------------------------------------------------------------------
methodFields :: [Dec] -> [VarBangType]
methodFields = mapMaybe \case
  SigD name (ForallT _ _ type_) ->
    Just ( methodFieldName name
         , Bang NoSourceUnpackedness NoSourceStrictness
         , type_
         )
  _ -> Nothing
 where
  methodFieldName (nameBase -> name@(c:_)) =
    mkName $ (if isAlpha_ c then '_' else '|'):name

-------------------------------------------------------------------------------
newtype c :=> a where
  Wants :: (c => a) -> c :=> a

instance Show (c :=> a) where
  show _ = "Wants{}"

-------------------------------------------------------------------------------
bndrToType :: TyVarBndr -> TH.Type
bndrToType = \case
  PlainTV n    -> VarT n
  KindedTV n k -> VarT n `SigT` k

-------------------------------------------------------------------------------
appHead :: TH.Type -> TH.Type
appHead = \case
  ForallT _ _ t -> appHead t
  AppT t _      -> appHead t
  SigT t _      -> appHead t
  -- TODO: can name be a type variable here?
  InfixT _ n _  -> ConT n
  UInfixT _ n _ -> ConT n
  ParensT t     -> appHead t
  t -> t

-------------------------------------------------------------------------------
isAlpha_ :: Char -> Bool
isAlpha_ c = isAlpha c || c == '_'

-- Tuples ---------------------------------------------------------------------
do let tupleCount = 15

   names <- traverse (newName . pure) $ take tupleCount ['a'..'z']

   let insts     = drop 2 $ zip [0..] (inits names)
       tupT i ts = foldl1' AppT $ TupleT i : ts

   pure $ insts <&> \(i, names') -> TySynInstD ''Inst $
     TySynEqn [tupT i $ VarT <$> names']
              (tupT i $ AppT (ConT ''Inst) . VarT <$> names')

-------------------------------------------------------------------------------
type instance Inst (a ~ b) = a :=: b

type role (:=:) nominal nominal
data a :=: b = UnsafeEquality

leibniz :: (forall f. f a -> f b) -> Inst (a ~ b)
leibniz f = f `seq` UnsafeEquality
