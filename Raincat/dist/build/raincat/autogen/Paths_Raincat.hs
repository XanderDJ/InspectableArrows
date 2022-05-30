{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Raincat (
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
version = Version [1,2,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/xander/.cabal/bin"
libdir     = "/home/xander/.cabal/lib/x86_64-linux-ghc-8.6.5/Raincat-1.2.1-CYXTmhZZ2T9G8rvkHGSG96-raincat"
dynlibdir  = "/home/xander/.cabal/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/xander/.cabal/share/x86_64-linux-ghc-8.6.5/Raincat-1.2.1"
libexecdir = "/home/xander/.cabal/libexec/x86_64-linux-ghc-8.6.5/Raincat-1.2.1"
sysconfdir = "/home/xander/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Raincat_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Raincat_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Raincat_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Raincat_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Raincat_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Raincat_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
