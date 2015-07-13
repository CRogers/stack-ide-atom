module Main where

import Test.HUnit
import Test.Tasty

import IdeSession
import TestSuite.State
import TestSuite.Tests.API
import TestSuite.Tests.Autocompletion
import TestSuite.Tests.BufferMode
import TestSuite.Tests.BuildDoc
import TestSuite.Tests.BuildExe
import TestSuite.Tests.BuildLicenses
import TestSuite.Tests.C
import TestSuite.Tests.CabalMacros
import TestSuite.Tests.Compilation
import TestSuite.Tests.Compliance
import TestSuite.Tests.Concurrency
import TestSuite.Tests.Crash
import TestSuite.Tests.Debugger
import TestSuite.Tests.FFI
import TestSuite.Tests.Integration
import TestSuite.Tests.InterruptRunExe
import TestSuite.Tests.InterruptRunStmt
import TestSuite.Tests.Issues
import TestSuite.Tests.Packages
import TestSuite.Tests.Performance
import TestSuite.Tests.Pseudoterminal
import TestSuite.Tests.SessionRestart
import TestSuite.Tests.SessionState
import TestSuite.Tests.SnippetEnvironment
import TestSuite.Tests.StdIO
import TestSuite.Tests.TH
import TestSuite.Tests.TypeInformation
import TestSuite.Tests.UpdateTargets

-- | Sanity check: make sure we can communicate with the server at all
-- and that we get the expected version
testGetGhcVersion :: TestSuiteEnv -> Assertion
testGetGhcVersion env = withAvailableSession env $ \session -> do
  version <- getGhcVersion session
  assertEqual ""  (testSuiteEnvGhcVersion env) version

allTests :: String -> TestSuiteEnv -> TestTree
allTests name env = testGroup name [
    stdTest env "getGhcVersion" testGetGhcVersion
  , testGroupIntegration        env
  , testGroupAPI                env
  , testGroupSessionState       env
  , testGroupCompilation        env
  , testGroupUpdateTargets      env
  , testGroupInterruptRunStmt   env
  , testGroupInterruptRunExe    env
  , testGroupStdIO              env
  , testGroupBufferMode         env
  , testGroupSnippetEnvironment env
  , testGroupTypeInformation    env
  , testGroupAutocompletion     env
  , testGroupFFI                env
  , testGroupC                  env
  , testGroupBuildExe           env
  , testGroupBuildDoc           env
  , testGroupBuildLicenses      env
  , testGroupCabalMacros        env
  , testGroupTH                 env
  , testGroupIssues             env
  , testGroupPackages           env
  , testGroupSessionRestart     env
  , testGroupCrash              env
  , testGroupCompliance         env
  , testGroupDebugger           env
  , testGroupConcurrency        env
  , testGroupPerformance        env
  , testGroupPseudoterminal     env
  ]

main :: IO ()
main =
    defaultMainWithIngredients ings $ testSuite $ \env ->
      let TestSuiteConfig{..} = testSuiteEnvConfig env
          env74  = env { testSuiteEnvGhcVersion = GHC_7_4  }
          env78  = env { testSuiteEnvGhcVersion = GHC_7_8  }
          env710 = env { testSuiteEnvGhcVersion = GHC_7_10 }
      in testGroup "IDE backend tests" $
           (if testSuiteConfigTest74  then [allTests "GHC 7.4"  env74]  else [])
        ++ (if testSuiteConfigTest78  then [allTests "GHC 7.8"  env78]  else [])
        ++ (if testSuiteConfigTest710 then [allTests "GHC 7.10" env710] else [])
  where
    ings = includingOptions testSuiteCommandLineOptions
         : defaultIngredients
