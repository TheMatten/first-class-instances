{-# LANGUAGE PatternSynonyms               #-}
{-# LANGUAGE TemplateHaskell               #-}

module MyTH
  ( makeMockable
  , Generic
  , mkMockableDict
  , Dict
  ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Char
import Data.Maybe
import Data.Traversable
import FCI.Internal
import GHC.Generics hiding (to)
import Language.Haskell.TH hiding (cxt)
import MockableImpl
import THStuff


makeMockable :: Name -> Q [Dec]
makeMockable name = do
  let opts =
        defaultOptions
          { mkInstClassConName = (++ "Impl") . mkInstClassConName defaultOptions
          , mkInstMethodFieldName = ('_' :) . mkInstMethodFieldName defaultOptions
          }
  dict_info <- getClassDictInfo opts name
  let class_name = className dict_info

  fmap join $ sequenceA
    [ mkInstWithOptions opts name
    , makePatternSyn dict_info
    , makeBetterFieldClass dict_info
          (getClassName class_name)
          (getMethodName class_name)
    , fmap pure $ makeMockableInstance dict_info
    ]


makePatternSyn :: ClassDictInfo -> DecsQ
makePatternSyn cdi = do
  let cxt          = dictConstraints cdi
      dcon_name    = dictConName cdi
      pattern_name = overName (reverse . drop 4 . reverse) $ dictConName cdi
      fields       = dictFields cdi
  let bndr_names = flip fmap (filter isMethodField fields) $ overName (drop 1) . fieldName
  let bndrs = fmap VarP bndr_names
      pats  = fmap (const WildP) (filter (not . isMethodField) fields)
           <> bndrs
      args = fmap (const $ VarE 'inst) (filter (not . isMethodField) fields)
          <> fmap VarE bndr_names
  pure
    [ PatSynD
        pattern_name
        (RecordPatSyn bndr_names)
        (ExplBidir [Clause bndrs (NormalB $ foldl AppE (ConE dcon_name) args) []])
        (ConP dcon_name pats)
    , PatSynSigD pattern_name
        . quantifyType' mempty cxt
        . foldr (:->) (getInstType cdi)
        . fmap origType
        $ filter isMethodField fields
    ]


makeBetterFieldClass :: ClassDictInfo -> Name -> Name -> DecsQ
makeBetterFieldClass cdi class_name method_name = do
  a_name <- newName "env"
  let cs = getClassStuff cdi
  Just m_name <- pure $ getTyVar $ csMType cs
  let vars = mapMaybe getTyVar (csArgs cs <> [csMType cs])
      fundeps = (FunDep [a_name] [m_name] :)
              . fmap (installANameIfNecessary m_name a_name)
              $ dictFundeps cdi
  pure
    [ ClassD [] class_name (fmap PlainTV $ vars <> [a_name]) fundeps
        . pure
        . SigD method_name
        $ VarT a_name :-> csInstType cs
    , makeBetterFieldInstance
        class_name
        (fmap VarT vars)
        (csInstType cs)
        method_name
        []
        (VarE 'id)
    ]


makeBetterFieldInstance
    :: Name
    -> [Type]
    -> Type
    -> Name
    -> [Pat]
    -> Exp
    -> Dec
makeBetterFieldInstance class_name args inst_ty method_name pats expr =
  InstanceD
      Nothing
      []
      (foldl AppT (ConT class_name) $ args ++ [inst_ty])
    [ FunD method_name
        . pure
        $ Clause pats (NormalB expr) []
    , PragmaD $ InlineP method_name Inlinable ConLike AllPhases
    ]


installANameIfNecessary :: Name -> Name -> FunDep -> FunDep
installANameIfNecessary m_name a_name fundep@(FunDep lhs rhs)
  | elem m_name lhs = FunDep (a_name : lhs) rhs
  | otherwise       = fundep


getTyVar :: Type -> Maybe Name
getTyVar (removeTyAnns -> VarT n) = Just n
getTyVar _                        = Nothing


-- class HasMonadFoo2 s m a | a m -> s where
--   getMonadFoo :: a -> Inst (MonadFoo s m)




firstToLower :: String -> String
firstToLower [] = []
firstToLower (x:xs) = toLower x : xs

getClassName :: Name -> Name
getClassName = overName ("Has" ++)

getMethodName :: Name -> Name
getMethodName = overName (("get" ++) . (++ "Dict"))


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
  VarE field_name `AppE` (VarE dict_name `AppE` VarE r_name)

--   foldl AppE (VarE 'view)
--     [ InfixE
--         (Just $ VarE dict_name)
--         (VarE '(.))
--         (Just $ VarE 'to `AppE` VarE field_name)
--     , VarE r_name
--     ]


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


makeMockableInstance :: ClassDictInfo -> Q Dec
makeMockableInstance cdi = do
  dict_name <- newName "dict"
  let ClassStuff m_type class_ctr _ args = getClassStuff cdi
  let class_name = className cdi
      okname = getMethodName class_name

  methods <- for (filter isMethodField $ dictFields cdi) $ \fi ->
    makeLiftedMethod
          m_type
          okname
          (origName fi)
          (fieldName fi)
      . init
      . splitArrowTs
      $ origType fi

  pure
    $ InstanceD
        Nothing
        ( foldl AppT (ConT (getClassName class_name)) (args ++ [m_type, VarT dict_name])
        : dictConstraints cdi
        ) (class_ctr `AppT` (ConT ''Mockable `AppT` VarT dict_name `AppT` m_type))
    $ join methods

data ClassStuff = ClassStuff
  { csMType       :: Type
  , csClassTyCtor :: Type
  , csInstType    :: Type
  , csArgs        :: [Type]
  }

getClassStuff :: ClassDictInfo -> ClassStuff
getClassStuff cdi =
  let class_name = className cdi
      class_vars = drop 1 $ splitAppTs $ dictTyArg cdi
      vars_to_keep = init class_vars
      m_type = removeSig $ last class_vars
      class_ctr = foldl AppT (ConT class_name) vars_to_keep
      inst_type = ConT ''Dict `AppT` (class_ctr `AppT` m_type)
   in ClassStuff m_type class_ctr inst_type vars_to_keep


getInstType :: ClassDictInfo -> Type
getInstType = csInstType . getClassStuff


isMethodField :: ClassDictField -> Bool
isMethodField = (== Method) . fieldSource


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



mkMockableDict :: Name -> DecsQ
mkMockableDict nm = do
  reify nm >>= \case
    TyConI (DataD _ tycon_name vars _ [con] _) -> do
      case con of
        NormalC con_name (fmap snd -> ts) ->
          makeHasDictInstForField tycon_name vars con_name ts
        RecC    con_name (fmap thd -> ts) ->
          makeHasDictInstForField tycon_name vars con_name ts
        _ -> error "Only for normal constructors and records"
    _ -> error "Must call it on a dang Type!"


makeHasDictInstForField :: Name -> [TyVarBndr] -> Name -> [Type] -> Q [Dec]
makeHasDictInstForField tycon_name vars con_name ts =
  for (zip ts [0..])
    . uncurry
    . hasDictInst tycon_name vars con_name
    $ length ts


isDict :: Type -> Bool
isDict t
  | removeTyAnns t == ConT ''Dict = True
  | removeTyAnns t == ConT ''Inst = True
  | otherwise                      = False

hasDictInst
    :: Name
    -> [TyVarBndr]
    -> Name
    -> Int
    -> Type
    -> Int
    -> DecQ
hasDictInst tycon_name bndrs con_name num t idx = do
  field <- newName "x"
  let pats =
        flip fmap [0..num - 1] $ \n ->
          case n == idx of
            True -> VarP field
            False -> WildP


  let apps = splitAppTs t
  case apps of
    [dict, c] | isDict dict -> do
      (ConT class_name : args) <- pure $ splitAppTs c
      pure $
        makeBetterFieldInstance
          (getClassName class_name)
          args
          (foldl AppT (ConT tycon_name) $ fmap (VarT . getBndrName) bndrs)
          (getMethodName class_name)
          [ConP con_name pats]
          (VarE field)
    _ -> error "shit"


thd :: (a, b, c) -> c
thd (_, _, c) = c

