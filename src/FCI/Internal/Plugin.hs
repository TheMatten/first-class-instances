module FCI.Internal.Plugin (plugin) where

import GhcPlugins

import FCI.Internal.Plugin.Core (fciCorePlugin)
import FCI.Internal.Plugin.TC   (fciTCPlugin)

-------------------------------------------------------------------------------
-- | Plugin required for first class families to work. Enable it by including
-- @{-\# options_ghc -fplugin=FCI \#-}@ pragma on top of source file or by
-- including @-fplugin=FCI@ flag in your project file.
plugin :: Plugin
plugin = defaultPlugin{
    installCoreToDos = fciCorePlugin
  , tcPlugin         = const $ Just fciTCPlugin
  , pluginRecompile  = purePlugin
  }

