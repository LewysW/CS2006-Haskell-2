module Paths_Gomoku (
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

bindir     = "/home/lewys/.cabal/bin"
libdir     = "/home/lewys/.cabal/lib/x86_64-linux-ghc-7.10.3/Gomoku-0.1.0.0-BklymqpoUNXEVG2w41aeTz"
datadir    = "/home/lewys/.cabal/share/x86_64-linux-ghc-7.10.3/Gomoku-0.1.0.0"
libexecdir = "/home/lewys/.cabal/libexec"
sysconfdir = "/home/lewys/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Gomoku_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Gomoku_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Gomoku_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Gomoku_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Gomoku_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
