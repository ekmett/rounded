module Main (main) where

import Distribution.Simple
import Distribution.Simple.Program
import Distribution.Simple.Program.Builtin
import Distribution.Simple.Program.Db
import Distribution.Simple.Program.Ar
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.PackageDescription
import Distribution.Verbosity

import System.IO
import System.Process
import System.FilePath
import System.Directory
import System.Exit

import Control.Applicative
import Control.Monad

import Data.List
import Data.Monoid
import Data.Maybe

-----------------------------------------------------------------
-- Horrible mess of semi-general code
--

-- TODO: support Windows nicely
runOrBomb :: FilePath -> [String] -> IO ()
runOrBomb cmd args = do
  (c, out, err) <- readProcessWithExitCode cmd args ""
  case c of
    ExitSuccess -> return ()
    ExitFailure e -> do
      hPutStrLn stderr $ "Command \"" ++ unwords (cmd:args) ++ "\" failed with exit code: " ++ show e
      hPutStrLn stdout $ out
      hPutStrLn stderr $ err
      exitWith $ ExitFailure e

getDist = canonicalizePath "dist" -- ugly. There must be a safer way to get the absolute path of the dist dir

createDirectory' dir = do
  exists <- doesDirectoryExist dir
  unless exists $ createDirectory dir -- let's hope nobody creates the directory first!

mpfrHooks :: UserHooks
mpfrHooks = autoconfUserHooks
    { preConf   = mpfrPreConf
    , postConf  = mpfrPostConf
    , postClean = mpfrPostClean
    }
  where
  -- We need to create the "include" directory at some point, but we're doing it this early to make cabal
  -- shut up about it not being present.
  mpfrPreConf args flags = do
    distDir <- getDist
    createDirectory' $ distDir </> "include"
    preConf autoconfUserHooks args flags

  -- Should we move this logic into configure.ac?
  mpfrPostConf args flags pkgd lbi = do
    distDir <- getDist
    putStrLn $ "Determining MPFR constants..."
    programExists <- doesFileExist $ distDir </> "mkMpfrDerivedConstants"
    unless programExists $ do
      Just gcc <- programFindLocation gccProgram normal
      runOrBomb gcc ["cbits/mkMpfrDerivedConstants.c", "-Impfr/src", "-I" ++ distDir </> "include", "-o", distDir </> "mkMpfrDerivedConstants"]
    headerExists <- doesFileExist $ distDir </> "include" </> "MpfrDerivedConstants.h"
    unless headerExists $ do
      header <- readProcess (distDir </> "mkMpfrDerivedConstants") [] ""
      writeFile (distDir </> "include" </> "MpfrDerivedConstants.h") header
    postConf autoconfUserHooks args flags pkgd lbi

  mpfrPostClean args flags pkg_descr _ = do
    readProcessWithExitCode "make" ["distclean"] ""
    return ()

main :: IO ()
main = defaultMainWithHooks mpfrHooks
