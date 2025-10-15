{-# LANGUAGE LambdaCase #-}

module Distribution.Simple.BuildWay (
    BuildWay (..),
    buildWayObjectExtension,
    buildWayInterfaceExtension,
) where

data BuildWay = StaticWay | DynWay | ProfWay | ProfDynWay | BytecodeWay
  deriving (Eq, Ord, Show, Read, Enum)

-- | Returns the object extension for the given build way (e.g. "dyn_o" for 'DynWay' on ELF)
buildWayObjectExtension :: String -> BuildWay -> String
buildWayObjectExtension ext = \case
  StaticWay -> ext
  ProfWay -> "p_" ++ ext
  DynWay -> "dyn_" ++ ext
  ProfDynWay -> "p_dyn_" ++ ext
  BytecodeWay -> "gbc"

buildWayInterfaceExtension :: BuildWay -> String
buildWayInterfaceExtension = \case
  StaticWay -> "hi"
  ProfWay -> "p_hi"
  DynWay -> "dyn_hi"
  ProfDynWay -> "p_dyn_hi"
  BytecodeWay -> "gbc_hi"
