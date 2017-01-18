{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Distribution.Client.CmdPath (
    pathCommand,
    pathAction,
  ) where

import Distribution.Simple.Setup
         ( HaddockFlags, fromFlagOrDefault )
import Distribution.Verbosity
         ( normal )
import Distribution.Simple.Command
         ( CommandUI(..), usageAlternatives, option, liftOption, OptionField, reqArg' )
import Distribution.Client.DistDirLayout
         ( DistDirLayout(..) )
import Distribution.Client.Setup
         ( GlobalFlags, ConfigFlags(..), ConfigExFlags, InstallFlags )
import Distribution.Client.ProjectOrchestration
         ( ProjectBuildContext(..), PreBuildHooks(..), runProjectPreBuildPhase )
import Distribution.Client.Types
         ( GenericReadyPackage(..) )
import Distribution.Client.BuildTarget
         ( readUserBuildTargets, resolveUserBuildTargets,
           buildTargetComponentName )
import Distribution.Client.ProjectPlanning.Types
         ( ElaboratedConfiguredPackage(..), ElaboratedPackageOrComponent(..),
           elabDistDirParams, compComponentName )

import qualified Distribution.Client.Setup as Client
import qualified Distribution.Client.InstallPlan as InstallPlan

import Control.Monad
import Data.List

-- | make a record for flags of various flags
type PathElement = String

pathCommand :: CommandUI ((ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags), [PathElement])
pathCommand = Client.installCommand {
  commandName         = "new-path",
  commandSynopsis     = "Display paths to various internal things",
  commandUsage        = usageAlternatives "new-paths" [ "[FLAGS]"
                                                      , "[FLAGS] TARGETS" ],
  commandDescription  = Nothing,
  commandNotes        = Nothing,
  commandDefaultFlags = (commandDefaultFlags Client.installCommand, []),
  commandOptions      = \sopa ->
      liftOptions fst helper (commandOptions Client.installCommand sopa) ++
      [ option [] ["path-element"]
          "Output only specific paths"
          snd (\ys' (x, ys) -> (x, ys ++ ys'))
          (reqArg' "ELEMENT" (:[]) id)
      ]
 }
  where
    helper x (_, z) = (x, z)

liftOptions :: (b -> a) -> (a -> b -> b)
            -> [OptionField a] -> [OptionField b]
liftOptions get set = map (liftOption get set)

pathAction :: ((ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags), [PathElement])
           -> [String] -> GlobalFlags -> IO ()
pathAction ((configFlags, configExFlags, installFlags, haddockFlags), pathElements)
            targetStrings globalFlags = do

    -- pathElements' = pathElements ^.. folded . folded
    print targetStrings
    print pathElements
    userTargets <- readUserBuildTargets targetStrings

    ProjectBuildContext {
        distDirLayout = DistDirLayout {..},
        elaboratedShared,
        elaboratedPlanToExecute = elaboratedPlan
      } <- runProjectPreBuildPhase
             verbosity
             ( globalFlags, configFlags, configExFlags, installFlags, haddockFlags )
             PreBuildHooks {
               hookPrePlanning      = \_ _ _ -> return (),
               hookSelectPlanSubset = \_ ep -> return ep
             }

    let localPackages =
          [ (elabPkgDescription elab, elabPkgSourceLocation elab)
          | InstallPlan.Configured elab <- InstallPlan.toList elaboratedPlan
          , elabLocalToProject elab
          ]

    buildTargets <- resolveUserBuildTargets localPackages userTargets


    let elabComponent (ElabComponent comp) = Just comp
        elabComponent (ElabPackage _)      = Nothing

        buildTargetsComp = map buildTargetComponentName buildTargets

        elabComponentName elab =
            compComponentName =<< elabComponent (elabPkgOrComp elab)

        plan = map (\(ReadyPackage elab) -> elab)
             $ InstallPlan.executionOrder elaboratedPlan

        Just elabs =
            forM buildTargetsComp $ \target ->
                find (\elab -> target == elabComponentName elab) plan

        -- TODO:
        --  - cabal file locations
        --  - project file location

        projectDirs = [
            ("dist-newstyle-dir", distDirectory),
            ("build-dir", distBuildRootDirectory),
            ("src-root", distUnpackedSrcRootDirectory),
            ("cache-dir", distProjectCacheDirectory),
            ("temp-dir", distTempDirectory),
            ("bin-dir", distBinDirectory)
            -- distPackageDB :: CompilerId -> PackageDB
         ]

        elabPaths elab =
            let distDirParams = elabDistDirParams elaboratedShared elab
            in [ ("dist-dir", distBuildDirectory distDirParams),
                 ("src-dir", distUnpackedSrcDirectory (elabPkgSourceId elab)),
                 ("cache-dist-dir", distPackageCacheDirectory distDirParams)
               ]

        paths :: [[(String, FilePath)]]
        paths | null targetStrings = [projectDirs]
              | otherwise          = map elabPaths elabs

    case pathElements of
        [pe] -> case lookup pe (head paths) of
            Just x -> putStrLn x
            Nothing -> fail "not implemented - xxx"
        _ -> do
            putStr $ unlines $ concat
                   $ map ((++ [""]) . map (\(l,p) -> l ++ ": " ++ p))
                   $ paths
  where
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
