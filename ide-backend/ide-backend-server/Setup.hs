import Distribution.Simple
import Distribution.Simple.LocalBuildInfo ( withPrograms, compiler )
import Distribution.Simple.Program ( runProgram, lookupProgram, ghcPkgProgram )
import Distribution.Simple.Setup ( fromFlag, configVerbosity, buildVerbosity )
import Distribution.Simple.Utils ( notice, die )

import Control.Exception ( bracket )
import Control.Monad ( join, when, forM_, liftM2 )
import Data.Bits ( xor )
import Data.Word ( Word )
import System.Directory ( getCurrentDirectory, setCurrentDirectory
                        , removeDirectoryRecursive, removeFile
                        , doesDirectoryExist, doesFileExist
                        , getDirectoryContents )
import System.Environment ( getArgs )
import System.FilePath ( (</>), takeExtension )

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


main :: IO ()
main = join $ liftM2 mainWith getArgs getCurrentDirectory

mainWith :: [String] -> FilePath -> IO ()
mainWith cmdLineArgs cwd = defaultMainWithHooksArgs hooks cmdLineArgs
  where
    hooks = simpleUserHooks {

      postConf  = \args flags pd lbi ->
        do configureRts lbi flags
           (postConf simpleUserHooks) args flags pd lbi

    , buildHook = \pd lbi hs flags ->
        do buildRts lbi flags
           makeRtsRelocatable lbi flags
           makeTarball "embedded-rts.tgz" cwd [dirToEmbed]
           (buildHook simpleUserHooks) pd lbi hs flags

    , preClean = \args flags ->
        do onRtsDir $ runSetupClean
           (preClean simpleUserHooks) args flags
    }


    dirToEmbed = "embedded-rts"
    pkgDbPath  = dirToEmbed </> "pkgdb"

    onRtsDir = withCurrentDirectory "ide-backend-rts"

    rawRunSetup    args  lbi verbosity = do
      let args' = args ++ [rtsBuildDirArg lbi]
      notice verbosity $ "Running cabal with " ++ show args'
      defaultMainArgs args'
    runSetup       cmd   lbi verbosity = rawRunSetup [cmd] lbi verbosity
    rerunSetupWith flags lbi verbosity = rawRunSetup (cmdLineArgs ++ flags) lbi verbosity
      -- NB. we must set builddir explicitly (and as the final arg)
      -- since when rerunning the command during configuration, the default
      -- builddir may be changed by a flag (e.g. as "stack" does), and if we
      -- are not aware of the right builddir on the subsequent calls, we won't
      -- find the configuration info and fail.

    rtsBuildDirArg lbi = "--builddir=dist/buildinfo-" ++ show (hashString $ show (compiler lbi))
      -- NB. We make the builddir a function of the compiler being used
      -- (approximated by the local build info). This is only relevant for
      -- people hacking on this library: we are trying to avoid the scenario
      -- where one first builds the server using compiler A and then, on the same
      -- directory, rebuilds the server using compiler B, but the "Setup build" step
      -- on the rts directory finds that the rts is already built (but with compiler A!),
      -- leaves it as is, and one ends up with a server for B with an embedded rts for A.
      -- Using a different builddir for A and B, this won't happen.
      -- Note also that we put all the local build dirs under the same root dist, so we
      -- know what to remove during cleanup

    runSetupClean = defaultMainArgs ["clean", "--builddir=dist"]


    -- Run "Setup configure" on the directory of the rts.
    -- We ensure that we pass exactly the same command line arguments
    -- we received (since those can be indicating the version of ghc
    -- with which we are compiling the server) but override the location
    -- of the output, since we want the package-db on "embedded-rts"
    configureRts lbi flags = do
      let verbosity = fromFlag (configVerbosity flags)
      notice verbosity "configuring rts..."

      let dirToEmbedFullPath = cwd </> dirToEmbed
          pkgDbFullPath = cwd </> pkgDbPath

      outOfTheWay dirToEmbed
      runGhcPkg ["init", pkgDbPath] lbi verbosity

      onRtsDir $
        rerunSetupWith [
            "--package-db=" ++  pkgDbFullPath
          , "--libdir="     ++ (dirToEmbedFullPath </> "lib")
          , "--bindir="     ++ (dirToEmbedFullPath </> "bin")
          , "--datadir="    ++ (dirToEmbedFullPath </> "share")
          , "--docdir="     ++ (dirToEmbedFullPath </> "doc")
          , "--htmldir="    ++ (dirToEmbedFullPath </> "doc")
          , "--haddockdir=" ++ (dirToEmbedFullPath </> "doc")
          , "--enable-library-for-ghci"
          ] lbi verbosity


    -- Builds the rts, which will end up in embedded-rts, and
    -- registers it to a package-db contained there as well
    buildRts lbi flags = do
      let verbosity = fromFlag (buildVerbosity flags)
      notice verbosity "building rts..."

      onRtsDir $ do
        runSetup "build" lbi verbosity

      notice verbosity "locally registering rts..."
      onRtsDir $ do
        runSetup "copy" lbi verbosity
        runSetup "register" lbi verbosity

    -- This hack is a workaround to Cabal not having yet (as of 1.22)
    -- a clear story regarding relocatable packages. We just replace
    -- every occurrence of 'cwd </> dirToEmbed' by the string "${pkgroot}"
    -- everywhere in the installed-package-conf and run 'ghc-pkg recache'
    -- afterwards.
    makeRtsRelocatable lbi flags = do
      let verbosity = fromFlag (buildVerbosity flags)
      notice verbosity "making rts a relocatable package..."

      pkgDb_files <- getDirectoryContents pkgDbPath
      let conf_files = filter ((== ".conf").takeExtension) pkgDb_files

      -- we expect only one conf file, if we couldn't find it the hack
      -- has failed  (better to detect here than at runtime!)
      when (null conf_files) $
        die "Couldn't file a conf file to hack"

      forM_ (map (pkgDbPath </>) conf_files) $ \conf_file -> do

        -- NB. ghc-pkg expects conf files to be in utf-8
        contents <- T.decodeUtf8 `fmap` BS.readFile conf_file

        -- we do a simple text substitution instead of parsing the conf file
        -- using Distribution.InstalledPackageInfo; the text substitution is
        -- safe enough and arguably more future-proof.
        let hack = T.replace (T.pack $ cwd </> dirToEmbed) (T.pack "${pkgroot}")
            new_contents = hack contents

        -- make sure that we have indeed made a substitution, otherwise this
        -- will blow at runtime....
        when (contents == new_contents) $
          die "No substitution, hack failed"

        BS.writeFile conf_file $ T.encodeUtf8 (new_contents)

      -- Update the package cache
      runGhcPkg ["recache", "--package-db=" ++ pkgDbPath] lbi verbosity

    runGhcPkg args lbi verb =
      let Just ghc_pkg = lookupProgram ghcPkgProgram (withPrograms lbi)
      in runProgram verb ghc_pkg args


-- Available in directory-1.2.3.0
withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory dir action =
  bracket getCurrentDirectory setCurrentDirectory $ \_ -> do
    setCurrentDirectory dir
    action

-- Removes the given file or directory
outOfTheWay :: FilePath -> IO ()
outOfTheWay fileOrDir = do
  is_dir <- doesDirectoryExist fileOrDir
  if is_dir
    then removeDirectoryRecursive fileOrDir
    else do is_file <- doesFileExist fileOrDir
            when is_file $
              removeFile fileOrDir

makeTarball :: FilePath -> FilePath -> [FilePath] -> IO ()
makeTarball tarball base contents =
  LBS.writeFile tarball . GZip.compress . Tar.write =<< Tar.pack base contents


-- Based on the Hashable String instance
hashString :: String -> Word
hashString = List.foldl' (\salt x -> salt `combine` hashChar x) 0x087fc72c
  where
    hashChar = fromIntegral . fromEnum
    combine h1 h2 = (h1 * 16777619) `xor` h2
