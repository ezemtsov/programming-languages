{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_scotty_test (
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

bindir     = "/Users/sy552tz/Desktop/programming-languages/haskell/scotty/.stack-work/install/x86_64-osx/lts-13.13/8.6.4/bin"
libdir     = "/Users/sy552tz/Desktop/programming-languages/haskell/scotty/.stack-work/install/x86_64-osx/lts-13.13/8.6.4/lib/x86_64-osx-ghc-8.6.4/scotty-test-0.1.0.0-Ck1Y6vAi7GqKsFjybLSdaN-scotty"
dynlibdir  = "/Users/sy552tz/Desktop/programming-languages/haskell/scotty/.stack-work/install/x86_64-osx/lts-13.13/8.6.4/lib/x86_64-osx-ghc-8.6.4"
datadir    = "/Users/sy552tz/Desktop/programming-languages/haskell/scotty/.stack-work/install/x86_64-osx/lts-13.13/8.6.4/share/x86_64-osx-ghc-8.6.4/scotty-test-0.1.0.0"
libexecdir = "/Users/sy552tz/Desktop/programming-languages/haskell/scotty/.stack-work/install/x86_64-osx/lts-13.13/8.6.4/libexec/x86_64-osx-ghc-8.6.4/scotty-test-0.1.0.0"
sysconfdir = "/Users/sy552tz/Desktop/programming-languages/haskell/scotty/.stack-work/install/x86_64-osx/lts-13.13/8.6.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "scotty_test_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "scotty_test_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "scotty_test_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "scotty_test_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "scotty_test_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "scotty_test_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
