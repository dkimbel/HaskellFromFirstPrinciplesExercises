{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_cipher (
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

bindir     = "/Users/dkimbel/Code/Haskell/First Principles/New/Ch14/cipher/.stack-work/install/x86_64-osx/lts-8.23/8.0.2/bin"
libdir     = "/Users/dkimbel/Code/Haskell/First Principles/New/Ch14/cipher/.stack-work/install/x86_64-osx/lts-8.23/8.0.2/lib/x86_64-osx-ghc-8.0.2/cipher-0.1.0.0-Dl43ufUPvrc1QXh284YUtq"
dynlibdir  = "/Users/dkimbel/Code/Haskell/First Principles/New/Ch14/cipher/.stack-work/install/x86_64-osx/lts-8.23/8.0.2/lib/x86_64-osx-ghc-8.0.2"
datadir    = "/Users/dkimbel/Code/Haskell/First Principles/New/Ch14/cipher/.stack-work/install/x86_64-osx/lts-8.23/8.0.2/share/x86_64-osx-ghc-8.0.2/cipher-0.1.0.0"
libexecdir = "/Users/dkimbel/Code/Haskell/First Principles/New/Ch14/cipher/.stack-work/install/x86_64-osx/lts-8.23/8.0.2/libexec"
sysconfdir = "/Users/dkimbel/Code/Haskell/First Principles/New/Ch14/cipher/.stack-work/install/x86_64-osx/lts-8.23/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "cipher_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cipher_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "cipher_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "cipher_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cipher_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cipher_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
