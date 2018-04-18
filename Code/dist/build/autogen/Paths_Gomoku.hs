{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_Gomoku (
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

bindir     = "/cs/home/locw/.cabal/bin"
libdir     = "/cs/home/locw/.cabal/lib/x86_64-linux-ghc-8.0.2/Gomoku-0.1.0.0-F5J28eJ9W6c9eGdLJSVLAK"
dynlibdir  = "/cs/home/locw/.cabal/lib/ghc-8.0.2/Gomoku-0.1.0.0-F5J28eJ9W6c9eGdLJSVLAK"
datadir    = "/cs/home/locw/.cabal/share/x86_64-linux-ghc-8.0.2/Gomoku-0.1.0.0"
libexecdir = "/cs/home/locw/.cabal/libexec"
sysconfdir = "/cs/home/locw/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Gomoku_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Gomoku_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Gomoku_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Gomoku_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Gomoku_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Gomoku_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
