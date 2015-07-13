module Main where

import Control.Exception (bracket)
import Control.Monad (liftM, unless)
import Data.Monoid (mconcat)
import System.Directory
import System.Environment
import System.FilePath.Find (always, extension, find)
import System.IO.Temp (withTempDirectory)

import IdeSession

--- A sample program using the library. It type-checks all files
--- in the given directory and prints out the list of errors.

-- | Some common extensions, etc. (please fill in).
-- Curiously "-XTypeFamilies" causes the type-checking of the default
-- test file to fail. The same file type-checks OK with the ghc-errors
-- test program (with no GHC extensions set).
defOpts :: [String]
defOpts = [ "-hide-all-packages"  -- make sure we don't depend on local pkgs
          , "-XCPP"
          , "-XNoTemplateHaskell"  -- TH not available when profiling
          , "-XBangPatterns"
          , "-XRecordWildCards"
          , "-XNamedFieldPuns"
          , "-XPatternGuards"
          , "-XScopedTypeVariables"
          , "-XMultiParamTypeClasses"
          , "-XRankNTypes"
-- causes problems with the Cabal code:          , "-XTypeFamilies"
          , "-XForeignFunctionInterface"
          , "-XDeriveDataTypeable"
          , "-package template-haskell"
          , "-package old-time"
          , "-package parallel"
          , "-package base"
          , "-package deepseq"
          , "-package filepath"
          , "-package directory"
          , "-package process"
          , "-package time"
          , "-package containers"
          , "-package array"
          , "-package pretty"
          , "-package bytestring"
          , "-package unix"
          ]

main :: IO ()
main = do
  args <- getArgs
  let (originalSourcesDir, opts) = case args of
        ["--help"] ->
          error "usage: typecheck-dir [source-dir [ghc-options]]"
        [dir] -> (dir, defOpts)
        dir : optsArg -> (dir, optsArg)
        [] -> ("TestSuite/inputs/Cabal-1.18.1.5",
               defOpts)
  slashTmp <- getTemporaryDirectory
  withTempDirectory slashTmp "typecheck-dir."
    $ check opts originalSourcesDir

check :: [String] -> FilePath -> FilePath -> IO ()
check opts what configDir = do
  putStrLn $ "Copying files from: " ++ what ++ "\n"
          ++ "to a temporary directory at: " ++ configDir ++ "\n"
  -- Init session.
  let sessionInitParams = defaultSessionInitParams {
                              sessionInitGhcOptions = opts
                            }
      sessionConfig     = defaultSessionConfig{
                              configDir
                         -- , configInProcess  = True
                            }

  bracket (initSession sessionInitParams sessionConfig)
          shutdownSession $ \session -> do
    isFile      <- doesFileExist      what
    isDirectory <- doesDirectoryExist what

    unless (isFile || isDirectory) $ fail ("invalid argument " ++ what)

    modules <- if isFile
      then return [what]
      else find always ((`elem` sourceExtensions) `liftM` extension) what

    print modules

    let update = mconcat (map updateSourceFileFromFile modules)

    updateSession session update print
    errs <- getSourceErrors session
    putStrLn $ "\nErrors and warnings:\n" ++ unlines (map show errs)
