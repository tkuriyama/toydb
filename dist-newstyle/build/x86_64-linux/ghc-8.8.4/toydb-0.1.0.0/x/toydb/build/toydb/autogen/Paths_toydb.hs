{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_toydb (
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

bindir     = "/home/daniel/.cabal/bin"
libdir     = "/home/daniel/.cabal/lib/x86_64-linux-ghc-8.8.4/toydb-0.1.0.0-inplace-toydb"
dynlibdir  = "/home/daniel/.cabal/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/daniel/.cabal/share/x86_64-linux-ghc-8.8.4/toydb-0.1.0.0"
libexecdir = "/home/daniel/.cabal/libexec/x86_64-linux-ghc-8.8.4/toydb-0.1.0.0"
sysconfdir = "/home/daniel/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "toydb_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "toydb_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "toydb_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "toydb_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "toydb_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "toydb_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
