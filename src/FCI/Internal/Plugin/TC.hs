module FCI.Internal.Plugin.TC (fciTCPlugin) where

import GhcPlugins
import TcRnTypes

-------------------------------------------------------------------------------
-- | FCI typechecker plugin that resolves use of 'Inst' family.
fciTCPlugin :: TcPlugin
fciTCPlugin = TcPlugin (pure ()) (const solveInst) (const $ pure ())

-------------------------------------------------------------------------------
solveInst :: TcPluginSolver
solveInst given derived wanted = do
  pprTraceM "Given: " $ ppr given
  pprTraceM "Derived: " $ ppr given
  pprTraceM "Wanted: " $ ppr given

  pure $ TcPluginOk [] []
