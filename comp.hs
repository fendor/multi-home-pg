{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

import Control.Monad.IO.Class
import GHC
import GHC.Driver.Types
import qualified Data.Map as Map
import GHC.Driver.Main
import GHC.Driver.Monad
import GHC.Unit.Types
import GHC.Driver.Session
import GHC.Utils.Outputable

componentA :: UnitId
componentA = stringToUnitId "main"
componentB :: UnitId
componentB =  stringToUnitId "componentB"

main :: IO ()
main = do
    runGhc' $ do
      dflags <- getSessionDynFlags
      liftIO $ putStrLn $ showSDoc dflags $ ppr componentA
      setTargets
        [ Target (TargetModule $ mkModuleName "A") componentA True Nothing
        , Target (TargetModule $ mkModuleName "B") componentB True Nothing
        ]
      load LoadAllTargets >>=
        \case
          Succeeded -> liftIO $ putStrLn "Succeeded"
          Failed -> do
            liftIO $ putStrLn "Failed"
            panic "Loading failed"
      hscEnv <- getSession
      liftIO $ putStrLn $ showSDoc (hsc_dflags hscEnv) (pprInternalUnitMap hscEnv)
      return ()

runGhc' :: Ghc a -> IO a
runGhc' act = do
    let libdir = "/home/munin/Documents/haskell/ghc/_build/stage1/lib"
    runGhc (Just libdir) $ do
      dflags0 <- getSessionDynFlags
      let dflagsBase = flip gopt_set Opt_WriteHie dflags0 {
              hscTarget = HscAsm
            , ghcLink   = LinkBinary
            , hiDir = Just "output"
            , objectDir = Just "output"
            , hieDir = Just "output"
            , verbosity = 1
            }
          dflagsA = dflagsBase
            { importPaths = ["./a"]
            , outputFile = Just "A"
            , thisUnitId = componentA
            }
          dflagsB = dflagsBase
            { importPaths = ["./b"]
            , thisUnitId = componentB
            }

      modifySession (\hsc_env -> hsc_env
          { hsc_currentPackage = componentA
          , hsc_internalUnitEnv =
              Map.fromList
                [ (componentA, InternalUnitEnv dflagsA emptyHomePackageTable)
                , (componentB, InternalUnitEnv dflagsB emptyHomePackageTable)
                ]
          }
        )
      act
