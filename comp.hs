{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

import Control.Monad.IO.Class
import GHC
import qualified Data.Map as Map
import GHC.Driver.Types
import GHC.Driver.Main
import GHC.Driver.Monad
import GHC.Unit.Types
import GHC.Driver.Session
import GHC.Utils.Outputable

componentA :: UnitId
componentA = stringToUnitId "componentA"

componentB :: UnitId
componentB =  stringToUnitId "componentB"

main :: IO ()
main = do
  runGhc' $ do
    liftIO $ writeFile "a/A.hs" $ unlines
      [ "module A where",
        "import B",
        ""
      , "main :: IO ()"
      , "main = do"
      , "  putStrLn \"Hello, World!\""
      , "  print foo"
      , "  print a"
      , "a = 1"
      ]
    liftIO $ writeFile "b/B.hs" $ unlines
      [ "module B where"
      , "foo = 2"
      ]
    setTargets
      [ Target (TargetModule $ mkModuleName "A") componentA True Nothing
      , Target (TargetModule $ mkModuleName "B") componentB True Nothing
      ]
    load LoadAllTargets >>=
      \case
        Succeeded -> liftIO $ putStrLn "Succeeded"
        Failed -> panic "Loading failed"
    hscEnv <- getSession
    liftIO $ putStrLn $ showSDoc (hsc_dflags hscEnv) (pprInternalUnitMap hscEnv)
    liftIO $ putStrLn $ showSDoc (hsc_dflags hscEnv) (ppr $ mgModSummaries $ hsc_mod_graph hscEnv)

    liftIO $ putStrLn ""
    liftIO $ putStrLn ""
    liftIO $ putStrLn "############################"
    liftIO $ putStrLn ""
    {--}
    liftIO $ writeFile "a/A.hs" $ unlines
      [ "module A where",
        "import B",
        ""
      , "main :: IO ()"
      , "main = do"
      , "  putStrLn \"Hello, World!\""
      , "  print foo"
      , "  print a"
      , "a = 1"
      ]
    --}
    liftIO $ writeFile "b/B.hs" $ unlines
      [ "module B where"
     , "foo = 2"
     , "bar = 4"
      ]

    load LoadAllTargets >>=
      \case
        Succeeded -> liftIO $ putStrLn "Succeeded"
        Failed -> panic "Loading failed"

    hscEnv <- getSession
    liftIO $ putStrLn $ showSDoc (hsc_dflags hscEnv) (pprInternalUnitMap hscEnv)
    liftIO $ putStrLn $ showSDoc (hsc_dflags hscEnv) (ppr $ mgModSummaries $ hsc_mod_graph hscEnv)

    return ()

runGhc' :: Ghc a -> IO a
runGhc' act = do
  let libdir = "/home/munin/Documents/haskell/ghc/_build/stage1/lib"
  runGhc (Just libdir) $ do
    dflags0 <- getSessionDynFlags
    let dflagsBase =
          flip gopt_set Opt_WriteHie
          --  $ flip dopt_set Opt_D_dump_rn_trace
          $ flip dopt_set Opt_D_dump_if_trace
          $ flip dopt_set Opt_D_dump_hi_diffs
          $ dflags0 {
                hscTarget = HscAsm
              , ghcLink   = LinkStaticLib
              , hiDir = Just "./output"
              , objectDir = Just "./output"
              , hieDir = Just "./output"
              , verbosity = 2
              , parMakeCount = Just 1
              }
        dflagsA = dflagsBase
          { importPaths = ["./a"]
          , homeUnitId = componentA
          , mainModIs = mkModule (stringToUnit $ unitIdString componentA) (mkModuleName "A")
          }
        dflagsB = dflagsBase
          { importPaths = ["./b"]
          , homeUnitId = componentB
          }

    modifySession (\hsc_env -> hsc_env
        { hsc_currentUnit = componentA
        , hsc_internalUnitEnv =
            Map.fromList
              [ (componentA, InternalUnitEnv dflagsA emptyHomePackageTable)
              , (componentB, InternalUnitEnv dflagsB emptyHomePackageTable)
              ]
        }
      )
    act
