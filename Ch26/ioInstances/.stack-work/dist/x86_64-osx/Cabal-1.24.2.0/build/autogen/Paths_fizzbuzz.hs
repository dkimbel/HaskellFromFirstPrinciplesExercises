{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_fizzbuzz (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/dkimbel/Code/Haskell/First Principles/New/Ch23/fizzbuzz/.stack-work/install/x86_64-osx/lts-9.0/8.0.2/bin"
libdir     = "/Users/dkimbel/Code/Haskell/First Principles/New/Ch23/fizzbuzz/.stack-work/install/x86_64-osx/lts-9.0/8.0.2/lib/x86_64-osx-ghc-8.0.2/fizzbuzz-0.1.0.0-IBWuXPxfFyWKZ90UVymEgf"
dynlibdir  = "/Users/dkimbel/Code/Haskell/First Principles/New/Ch23/fizzbuzz/.stack-work/install/x86_64-osx/lts-9.0/8.0.2/lib/x86_64-osx-ghc-8.0.2"
datadir    = "/Users/dkimbel/Code/Haskell/First Principles/New/Ch23/fizzbuzz/.stack-work/install/x86_64-osx/lts-9.0/8.0.2/share/x86_64-osx-ghc-8.0.2/fizzbuzz-0.1.0.0"
libexecdir = "/Users/dkimbel/Code/Haskell/First Principles/New/Ch23/fizzbuzz/.stack-work/install/x86_64-osx/lts-9.0/8.0.2/libexec"
sysconfdir = "/Users/dkimbel/Code/Haskell/First Principles/New/Ch23/fizzbuzz/.stack-work/install/x86_64-osx/lts-9.0/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "fizzbuzz_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "fizzbuzz_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "fizzbuzz_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "fizzbuzz_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "fizzbuzz_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "fizzbuzz_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
