module Paths_shawty (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/dkimbel/Code/Haskell/First Principles/New/Ch22/shawty-prime-master/.stack-work/install/x86_64-osx/lts-3.7/7.10.2/bin"
libdir     = "/Users/dkimbel/Code/Haskell/First Principles/New/Ch22/shawty-prime-master/.stack-work/install/x86_64-osx/lts-3.7/7.10.2/lib/x86_64-osx-ghc-7.10.2/shawty-0.1.0.0-IYcrk48IyEoLu1TUuqMVX5"
datadir    = "/Users/dkimbel/Code/Haskell/First Principles/New/Ch22/shawty-prime-master/.stack-work/install/x86_64-osx/lts-3.7/7.10.2/share/x86_64-osx-ghc-7.10.2/shawty-0.1.0.0"
libexecdir = "/Users/dkimbel/Code/Haskell/First Principles/New/Ch22/shawty-prime-master/.stack-work/install/x86_64-osx/lts-3.7/7.10.2/libexec"
sysconfdir = "/Users/dkimbel/Code/Haskell/First Principles/New/Ch22/shawty-prime-master/.stack-work/install/x86_64-osx/lts-3.7/7.10.2/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "shawty_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "shawty_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "shawty_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "shawty_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "shawty_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
