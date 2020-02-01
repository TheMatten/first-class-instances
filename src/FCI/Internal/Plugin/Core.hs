{-# language TemplateHaskellQuotes #-}

module FCI.Internal.Plugin.Core (fciCorePlugin) where

import           Control.Monad
import           Data.Bool
import           Data.Generics             hiding (TyCon, empty)
import           Data.Maybe
import           GhcPlugins
import qualified Language.Haskell.TH as TH

import FCI.Internal.Definitions (Dict, inst, (==>))

-------------------------------------------------------------------------------
-- | FCI Core plugin that injects pass replacing primitives ('inst', (==>)).
fciCorePlugin :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
fciCorePlugin opts = pure . (CoreDoPluginPass "FCI" (replaceFCIPrims opts) :)

-------------------------------------------------------------------------------
-- See [FCI primitives] in 'FCI.Internal.Definitions'.
replaceFCIPrims :: [CommandLineOption] -> ModGuts -> CoreM ModGuts
replaceFCIPrims (elem "debug" -> isDebug) mg = do
  let go = replaceFill isDebug <=< replaceInst isDebug
  mg_binds <- everywhereM (mkM go) $ mg_binds mg
  pure mg{ mg_binds }

-------------------------------------------------------------------------------
replaceInst, replaceFill :: Bool {- ^ debug? -} -> Expr Var -> CoreM (Expr Var)

-- |
-- @
-- inst @c $d
-- ===>
-- $d `cast` UnsafeCo nominal c (Inst c)
-- @
replaceInst isDebug = \case
  x@(Var i `App` Type c `App` Var d) -> do
    eqTHNameVar 'inst i >>= bool (pure x) do
      fciLog isDebug $ hsep [ text "found inst:"
        , ppr i , parens (ppr c) , ppr d
        ]
      _DictTy <- _DictTyM c
      pure $ Var d `Cast` mkUnsafeCo Nominal c _DictTy
  x -> pure x

-- |
-- @
-- (==>) @c @t d r
-- ===>
-- r (d `cast` UnsafeCo nominal (Inst c) c)
-- @
replaceFill isDebug = \case
  x@(Var f `App` Type c `App` Type t `App` d `App` r) -> do
    eqTHNameVar '(==>) f >>= bool (pure x) do
      fciLog isDebug $ hsep [ text "found (==>):"
        , parens $ ppr f, parens $ ppr c
                        , parens $ ppr t
                        , parens $ ppr d
                        ,          ppr r
        ]
      _DictTy <- _DictTyM c
      pure $ r `App` d `Cast` mkUnsafeCo Nominal c _DictTy
  x -> pure x

-------------------------------------------------------------------------------
eqTHNameVar :: TH.Name -> Var -> CoreM Bool
eqTHNameVar n v = do
  n' <- thToGHCName n
  pure $ n' == varName v

-------------------------------------------------------------------------------
_DictTyM :: Type -> CoreM Type
_DictTyM t = do
  _InstTyCon <- lookupTyCon =<< thToGHCName ''Dict
  pure $ mkTyConApp _InstTyCon [t]

-------------------------------------------------------------------------------
thToGHCName :: TH.Name -> CoreM Name
thToGHCName = fmap (fromMaybe cannotFind) . thNameToGhcName where
  cannotFind = error "FCI.plugin: cannot find quoted name"

-------------------------------------------------------------------------------
fciLog :: Bool {- ^ debug? -} -> SDoc -> CoreM ()
fciLog False _   = pure ()
fciLog True  doc = putMsg $ text "[FCI]" <+> doc
