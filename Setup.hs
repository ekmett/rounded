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

-- Not particularly thread safe, but the whole notion of a current directory isn't either
inDirectory :: FilePath -> IO r -> IO r
inDirectory dir action = do
  old <- getCurrentDirectory
  setCurrentDirectory dir
  res <- action
  setCurrentDirectory old
  return res

mpfrVersion = "3.1.0"
mpfrRoot = "deps/mpfr-" ++ mpfrVersion

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

getDist = do
  -- let Flag relDistDir = Flag defaultDistPref `mappend` configDistPref flags
  -- canonicalizePath relDistDir
  canonicalizePath "dist" -- ugly. There must be a safer way to get the absolute path of the dist dir

createDirectory' dir = do
  exists <- doesDirectoryExist $ dir
  unless exists $ createDirectory dir -- let's hope nobody creates the directory first!

configureMpfr :: FilePath -> IO ()
configureMpfr distDir =
  inDirectory mpfrRoot $ do
    putStrLn $ "--> Configuring MPFR " ++ mpfrVersion ++ "..."
    runOrBomb "sh" ["configure", "--prefix=" ++ distDir]

makeMpfr :: FilePath -> IO ()
makeMpfr distDir =
  inDirectory mpfrRoot $ do
    putStrLn $ "--> Building MPFR " ++ mpfrVersion ++ "..."
    runOrBomb "make" ["-j6"]
    runOrBomb "make" ["install"]

mpfrHooks :: UserHooks
mpfrHooks = autoconfUserHooks
    { preConf   = mpfrPreConf
    , postConf  = mpfrPostConf
    , confHook  = mpfrConfHook
    , preBuild  = mpfrPreBuild
    , postBuild = mpfrPostBuild
    , postClean = mpfrPostClean
    }
  where
  mpfrConfHook (pkg, pbi) flags = do
    distDir <- getDist
    lbi <- confHook autoconfUserHooks (pkg, pbi) flags
    let lpd = localPkgDescr lbi
        lib = fromJust (library lpd)
        libbi = libBuildInfo lib
        libbi' = libbi { extraLibDirs = (distDir </> "lib") : extraLibDirs libbi }
        lib' = lib { libBuildInfo = libbi' }
        lpd' = lpd { library = Just lib' }
    return lbi { localPkgDescr = lpd' }
 
  -- We need to create the "include" directory at some point, but we're doing it this early to make cabal
  -- shut up about it not being present.
  mpfrPreConf args flags = do
    distDir <- getDist
    createDirectory' $ distDir </> "include"
    createDirectory' $ distDir </> "lib"
    createDirectory' $ distDir </> "tmp"
    return emptyHookedBuildInfo

  mpfrPostConf args flags pkg_descr lbi = do
    postConf simpleUserHooks args flags pkg_descr lbi
    configureMpfr =<< getDist

  mpfrPreBuild args flags = do
    preBuild simpleUserHooks args flags
    distDir <- getDist
    makeMpfr distDir
    putStrLn $ "Determining MPFR constants..."
    programExists <- doesFileExist $ distDir </> "mkMpfrDerivedConstants"
    unless programExists $ do
      Just gcc <- programFindLocation gccProgram normal
      runOrBomb gcc ["cbits/mkMpfrDerivedConstants.c", "-I" ++ distDir </> "include", "-o", distDir </> "mkMpfrDerivedConstants"]
    headerExists <- doesFileExist $ distDir </> "include" </> "MpfrDerivedConstants.h"
    unless headerExists $ do
      header <- readProcess (distDir </> "mkMpfrDerivedConstants") [] ""
      writeFile (distDir </> "include" </> "MpfrDerivedConstants.h") header
    return emptyHookedBuildInfo

  mpfrPostBuild args flags pkg_descr lbi = do
    distDir <- getDist
    (ar, _) <- requireProgram silent arProgram defaultProgramDb

    putStrLn "Mangling static library..."
    inDirectory (distDir </> "tmp") $ do
      runOrBomb "ar" ["-x", distDir </> "build" </> "libHSrounded-0.1.a"]
      runOrBomb "ar" ["-x", distDir </> "lib" </> "libmpfr.a"]

    objects <- map ((distDir </> "tmp") </>) <$> filter (".o" `isSuffixOf`) <$> getDirectoryContents (distDir </> "tmp")

    createArLibArchive silent ar (distDir </> "build" </> "libHSrounded-0.1.a") objects

    putStrLn "Mangling static library (prof)..."
    inDirectory (distDir </> "tmp") $ do
      runOrBomb "ar" ["-x", distDir </> "build" </> "libHSrounded-0.1_p.a"]
      runOrBomb "ar" ["-x", distDir </> "lib" </> "libmpfr.a"]

    objects <- map ((distDir </> "tmp") </>) <$> filter (".o" `isSuffixOf`) <$> getDirectoryContents (distDir </> "tmp")

    createArLibArchive silent ar (distDir </> "build" </> "libHSrounded-0.1_p.a") objects

    postBuild simpleUserHooks args flags pkg_descr lbi

  mpfrPostClean args flags pkg_descr _ = do
    inDirectory mpfrRoot (readProcessWithExitCode "make" ["distclean"] "")
    return ()

main :: IO ()
main = defaultMainWithHooks mpfrHooks
