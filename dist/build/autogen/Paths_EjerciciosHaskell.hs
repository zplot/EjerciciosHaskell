module Paths_EjerciciosHaskell (
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
version = Version {versionBranch = [1,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/luisfontes/Library/Haskell/bin"
libdir     = "/Users/luisfontes/Library/Haskell/ghc-7.8.3-x86_64/lib/EjerciciosHaskell-1.0"
datadir    = "/Users/luisfontes/Library/Haskell/share/ghc-7.8.3-x86_64/EjerciciosHaskell-1.0"
libexecdir = "/Users/luisfontes/Library/Haskell/libexec"
sysconfdir = "/Users/luisfontes/Library/Haskell/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "EjerciciosHaskell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "EjerciciosHaskell_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "EjerciciosHaskell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "EjerciciosHaskell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "EjerciciosHaskell_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
