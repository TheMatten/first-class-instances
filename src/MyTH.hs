{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module MyTH
  ( makeMockable
  , makeFields
  , Generic
  ) where

import FCI.Internal
import Data.Char
import Control.Lens
import Language.Haskell.TH hiding (cxt)
import Language.Haskell.TH.Lens hiding (name)
import Data.List
import Control.Lens.Internal.TH
import Control.Monad
import qualified Data.Set as Set
import Data.Set (Set)
import MockableImpl
import Control.Arrow ((>>>))
import Control.Monad.Trans.Reader
import Data.Traversable
import GHC.Generics hiding (to)
import Control.Monad.Trans.Class


makeMockable :: Name -> Q [Dec]
makeMockable name = do
  let opts =
        defaultOptions
          { mkInstClassConName = (++ "Impl") . mkInstClassConName defaultOptions
          }
  dict_info <- getClassDictInfo opts name
  let class_name = className dict_info

  fmap join $ sequenceA
    [ mkInstWithOptions opts name
    , fmap pure $
        makeFieldClass
          (getClassName class_name)
          (getMethodName class_name)
    , makePatternSyn dict_info
    , fmap pure $ makeMockableInstance dict_info
    ]


makePatternSyn :: ClassDictInfo -> DecsQ
makePatternSyn cdi = do
  let cxt          = dictConstraints cdi
      dcon_name    = dictConName cdi
      pattern_name = overName (reverse . drop 4 . reverse) $ dictConName cdi
      fields       = dictFields cdi
  bndr_names <- for (filter isMethodField fields) $ const $ newName "x"
  let bndrs = fmap VarP bndr_names
      pats  = fmap (const WildP) (filter (not . isMethodField) fields)
           <> bndrs
      args = fmap (const $ VarE 'inst) (filter (not . isMethodField) fields)
          <> fmap VarE bndr_names

  pure
    [ PatSynD
        pattern_name
        (PrefixPatSyn bndr_names)
        (ExplBidir [Clause bndrs (NormalB $ foldl AppE (ConE dcon_name) args) []])
        (ConP dcon_name pats)
    , PatSynSigD pattern_name
        . quantifyType' mempty cxt
        . foldr (:->) (getInstType cdi)
        . fmap origType
        $ filter isMethodField fields
    ]


makeFieldClass :: Name -> Name -> DecQ
makeFieldClass className methodName =
  classD (pure []) className [PlainTV s, PlainTV a] [FunDep [s] [a]]
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


diddleArg :: Type -> Name -> Position -> (Exp, Type) -> Q Exp
diddleArg m_type r_name pos (arg_exp, arg_type) =
  -- trace (show (pos, arg, arg_type)) $
  case classifyArg m_type arg_type of
    Boring  -> pure arg_exp
    Monadic -> pure $ liftCorrectPosition r_name pos arg_exp
    Lambda -> do
      let arrows      = splitArrowTs arg_type
          arg_types   = init arrows
          result_type = last arrows
      arg_names <- for arg_types $ const $ newName "x"
      splice_args <- for (zip (fmap VarE arg_names) arg_types) $ \arg -> do
        diddleArg m_type r_name (negatePosition pos) arg
      let call = foldl AppE arg_exp splice_args
      splice_result <- diddleArg m_type r_name pos (call, result_type)
      pure $ LamE (fmap VarP arg_names) splice_result


liftCorrectPosition :: Name -> Position -> Exp -> Exp
liftCorrectPosition r_name Negative arg =
  VarE 'coerceMockable `AppE` arg `AppE` VarE r_name
liftCorrectPosition _ Positive arg =
  VarE 'lift `AppE` arg


------------------------------------------------------------------------------
-- | Builds an expression of the form
--
-- @r_name ^. dict_name . to field_name@
makeHasDictLookup :: Name -> Name -> Name -> Exp
makeHasDictLookup r_name dict_name field_name =
  foldl AppE (VarE 'view)
    [ InfixE
        (Just $ VarE dict_name)
        (VarE '(.))
        (Just $ VarE 'to `AppE` VarE field_name)
    , VarE r_name
    ]


------------------------------------------------------------------------------
-- | Lifts a call to @field_name@ with @args@, by coercing them if they are in
-- the @m_type@ monad, and ignoring them othewise.
makeLiftedCall :: Type -> Name -> Name -> Name -> [(Name, Type)] -> Q Exp
makeLiftedCall m_type r_name dict_name field_name args = do
  args' <- for args $ \(arg, ty) -> diddleArg m_type r_name Negative (VarE arg, ty)
  pure $ foldl AppE (makeHasDictLookup r_name dict_name field_name) args'


------------------------------------------------------------------------------
-- | Builds an expression of the form
--
-- @Mockable $ ReaderT $ \r_name -> expr@
--
-- where @expr@ is expected to be the result of a 'makeLiftedCall'.
makeMockableScaffold :: Name -> Exp -> Exp
makeMockableScaffold r_name expr =
  ConE 'Mockable `AppE` (ConE 'ReaderT `AppE` LamE [VarP r_name] expr)


------------------------------------------------------------------------------
-- | Lifts a method call by calling 'makeMockableScaffold' and 'makeLiftedCall'
-- by generating the @r_name@ and argument names.
makeLiftedMethod :: Type -> Name -> Name -> Name -> [Type] -> Q [Dec]
makeLiftedMethod m_type dict_name method_name field_name arg_types = do
  arg_names <- for arg_types $ const $ newName "x"
  r_name <- newName "r"
  let args = zip arg_names arg_types
  call <- makeLiftedCall m_type r_name dict_name field_name args
  pure
    [ FunD method_name
      $ pure
      $ Clause (fmap VarP arg_names)
          (NormalB
            $ makeMockableScaffold r_name call
          ) []
    , PragmaD $ InlineP method_name Inlinable FunLike AllPhases
    ]


------------------------------------------------------------------------------
-- | Given:

-- @@
-- class Monad m => MonadState s m | m -> s where
--   get :: m s
--   put :: s -> m ()
-- makeMockable ''MonadState
-- @@
--
-- generate the following:
--
-- @@
-- instance ( HasMonadState (dict m) (Inst (MonadState s m))
--          , Monad m
--          ) =>
--       MonadState s (Mockable dict m) where
--   get
--     = Mockable
--         (ReaderT
--            (\ r_a3DNQ -> ((^.) r_a3DNQ) (((.) monadState) (to _get))))
--   {-# INLINABLE get #-}
--
--   put x_a3DNR
--     = Mockable
--         (ReaderT
--            (\ r_a3DNS
--               -> (((^.) r_a3DNS) (((.) monadState) (to _put))) x_a3DNR))
--   {-# INLINABLE put #-}
-- @@
makeMockableInstance :: ClassDictInfo -> Q Dec
makeMockableInstance cdi = do
  dict_name <- newName "dict"
  let class_name = className cdi
      class_vars = splitAppTs $ dictTyArg cdi
      vars_to_keep = drop 1 $ init class_vars
      m_type = removeSig $ last class_vars
      class_ctr = foldl AppT (ConT class_name) vars_to_keep
      okname = getMethodName class_name

  methods <- for (filter isMethodField $ dictFields cdi) $ \fi -> do
    makeLiftedMethod m_type okname (origName fi) (fieldName fi) $ init $ splitArrowTs $ origType fi

  pure
    $ InstanceD
        Nothing
        ( ConT (getClassName class_name)
            `AppT` (VarT dict_name `AppT` m_type)
            `AppT` getInstType cdi
        : dictConstraints cdi
        ) (class_ctr `AppT` (ConT ''Mockable `AppT` VarT dict_name `AppT` m_type)) $ join methods


getInstType :: ClassDictInfo -> Type
getInstType cdi =
  let class_name = className cdi
      class_vars = splitAppTs $ dictTyArg cdi
      vars_to_keep = drop 1 $ init class_vars
      m_type = removeSig $ last class_vars
      class_ctr = foldl AppT (ConT class_name) vars_to_keep
   in ConT ''Inst `AppT` (class_ctr `AppT` m_type)


isMethodField :: ClassDictField -> Bool
isMethodField = (== Method) . fieldSource

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
  t :-> ts -> t : splitArrowTs ts
  t        -> [t]


removeSig :: Type -> Type
removeSig (SigT t _) = t
removeSig t = t


data Position
  = Positive
  | Negative
  deriving (Eq, Ord, Show)

negatePosition :: Position -> Position
negatePosition Positive = Negative
negatePosition Negative = Positive

data ArgType
  = Boring
  | Monadic
  | Lambda
  deriving (Eq, Ord, Show)


classifyArg :: Type -> Type -> ArgType
classifyArg _ (_ :-> _) = Lambda
classifyArg m_type arg_type
  | m_type == head (splitAppTs arg_type) = Monadic
  | otherwise = Boring


pattern (:->) :: Type -> Type -> Type
pattern t :-> ts <- AppT (AppT ArrowT t) ts
  where
    t :-> ts = AppT (AppT ArrowT t) ts




