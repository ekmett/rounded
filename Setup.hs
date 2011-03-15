module Main (main) where

import Distribution.Simple
import Distribution.Simple.Program
import Distribution.Verbosity

import System.IO
import System.Process
import System.Directory

import Control.Monad

main :: IO ()
main = 
  do Just gcc <- programFindLocation gccProgram normal
     -- TODO fix -m32 and /opt/local/include depending on platform
     rawSystem gcc ["cbits/mkMpfrDerivedConstants.c", "-I/opt/local/include", "-o", "dist/mkMpfrDerivedConstants"]
     exists <- doesDirectoryExist "dist/include"
     unless exists $ createDirectory "dist/include/"
     header <- readProcess "dist/mkMpfrDerivedConstants" [] ""
     writeFile "dist/include/MpfrDerivedConstants.h" header
     defaultMain
