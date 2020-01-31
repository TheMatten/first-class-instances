{-# LANGUAGE TemplateHaskell #-}

module MyTH
  ( makeMockable
  , makeFields
  , Generic
  ) where

import Data.Char
import FCI.Internal.TH
import FCI.Internal.Types
import Control.Lens
import Language.Haskell.TH
import Language.Haskell.TH.Lens hiding (name)
import Data.List
import Control.Lens.Internal.TH
import Control.Monad
import qualified Data.Set as Set
import Data.Set (Set)
import Mockable
import Control.Arrow ((>>>))
import Control.Monad.Trans.Reader
import Data.Traversable
import GHC.Generics hiding (to)


makeMockable :: Name -> Q [Dec]
makeMockable name = do
  dict_info <- getClassDictInfo name
  let class_name = className dict_info

  fmap join $ sequenceA
    [ mkInst name
    , fmap pure $
        makeFieldClass
          (getClassName class_name)
          (getMethodName class_name)
    , fmap pure $ makeMockableInstance dict_info
    ]

makeFieldClass :: Name -> Name -> DecQ
makeFieldClass className methodName =
  classD (cxt []) className [PlainTV s, PlainTV a] [FunDep [s] [a]]
         [sigD methodName (return methodType)]
  where
  methodType = quantifyType' (Set.fromList [s,a])
                             []
             $ ''Lens' `conAppsT` [VarT s,VarT a]
  s = mkName "s"
  a = mkName "a"


-- | This function works like 'quantifyType' except that it takes
-- a list of variables to exclude from quantification.
quantifyType' :: Set Name -> Cxt -> Type -> Type
quantifyType' exclude c t = ForallT vs c t
  where
  vs = map PlainTV
     $ filter (`Set.notMember` exclude)
     $ nub -- stable order
     $ toListOf typeVars t

overName :: (String -> String) -> Name -> Name
overName f = mkName . f . nameBase

firstToLower :: String -> String
firstToLower [] = []
firstToLower (x:xs) = toLower x : xs

getClassName :: Name -> Name
getClassName = overName ("Has" ++)

getMethodName :: Name -> Name
getMethodName = overName firstToLower


coerceArgIfNecessary :: Type -> Name -> (Name, Type) -> Exp
coerceArgIfNecessary m_type r_name (arg_name, arg_type)
  -- | otherwise = error $ show (m_type, splitAppTs arg_type)
  | m_type /= head (splitAppTs arg_type) = VarE arg_name
  | otherwise =
      VarE 'coerceMockable `AppE` VarE arg_name `AppE` VarE r_name


makeThing :: Name -> Name -> Name -> Exp
makeThing r_name dict_name field_name =
  foldl AppE (VarE '(^.))
    [ VarE r_name
    , foldl AppE (VarE '(.))
        [ VarE dict_name
        , VarE 'to `AppE` VarE field_name
        ]
    ]

makeLifted :: Type -> Name -> Name -> Name -> [(Name, Type)] -> Exp
makeLifted m_type r_name dict_name field_name args =
  foldl AppE (makeThing r_name dict_name field_name)
    $ fmap (coerceArgIfNecessary m_type r_name) args

makeScaffold :: Name -> Exp -> Exp
makeScaffold r_name lifted =
  ConE 'Mockable `AppE` (ConE 'ReaderT `AppE` LamE [VarP r_name] lifted)

makeLiftedMethod :: Type -> Name -> Name -> Name -> [Type] -> Q [Dec]
makeLiftedMethod m_type dict_name method_name field_name arg_types = do
  arg_names <- for arg_types $ const $ newName "x"
  r_name <- newName "r"
  let args = zip arg_names arg_types
  pure
    [ FunD method_name
      $ pure
      $ Clause (fmap VarP arg_names)
          (NormalB
            $ makeScaffold r_name
            $ makeLifted m_type r_name dict_name field_name args
          ) []
    , PragmaD $ InlineP method_name Inlinable FunLike AllPhases
    ]


makeMockableInstance :: ClassDictInfo -> Q Dec
makeMockableInstance cdi = do
  dict_name <- newName "dict"
  let class_name = className cdi
      class_vars = splitAppTs $ dictTyArg cdi
      vars_to_keep = drop 1 $ init class_vars
      m_type = removeSig $ last class_vars
      class_ctr = foldl AppT (ConT class_name) vars_to_keep
      okname = getMethodName class_name

  methods <- for (dictFields cdi) $ \fi -> do
    makeLiftedMethod m_type okname (origName fi) (fieldName fi) $ init $ splitArrowTs $ origType fi


  pure
    $ InstanceD
        Nothing
        ( ConT (getClassName class_name)
            `AppT` (VarT dict_name `AppT` m_type)
            `AppT` (ConT ''Inst `AppT` (class_ctr `AppT` m_type))
        : dictConstraints cdi
        ) (class_ctr `AppT` (ConT ''Mockable `AppT` VarT dict_name `AppT` m_type)) $ join methods

-- instance HasMonadFoo (dict m) (Inst (MonadFoo m)) => MonadFoo (Mockable dict m) where
--   foo = Mockable $ ReaderT $ \r ->
--     (r ^. monadFoo . to _foo)




------------------------------------------------------------------------------
removeTyAnns :: Type -> Type
removeTyAnns = \case
  ForallT _ _ t -> removeTyAnns t
  SigT t _      -> removeTyAnns t
  ParensT t     -> removeTyAnns t
  t -> t

------------------------------------------------------------------------------
splitAppTs :: Type -> [Type]
splitAppTs = removeTyAnns >>> \case
  t `AppT` arg -> splitAppTs t ++ [arg]
  t            -> [t]

------------------------------------------------------------------------------
splitArrowTs :: Type -> [Type]
splitArrowTs = removeTyAnns >>> \case
  AppT (AppT ArrowT t) ts -> t : splitArrowTs ts
  t                       -> [t]


removeSig :: Type -> Type
removeSig (SigT t _) = t
removeSig t = t

