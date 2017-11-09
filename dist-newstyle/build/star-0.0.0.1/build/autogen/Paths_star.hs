{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_star (
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
version = Version [0,0,0,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/chessai/.cabal/bin"
libdir     = "/home/chessai/.cabal/lib/x86_64-linux-ghc-8.2.0.20170507/.fake.star-0.0.0.1"
dynlibdir  = "/home/chessai/.cabal/lib/x86_64-linux-ghc-8.2.0.20170507"
datadir    = "/home/chessai/.cabal/share/x86_64-linux-ghc-8.2.0.20170507/star-0.0.0.1"
libexecdir = "/home/chessai/.cabal/libexec"
sysconfdir = "/home/chessai/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "star_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "star_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "star_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "star_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "star_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "star_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
