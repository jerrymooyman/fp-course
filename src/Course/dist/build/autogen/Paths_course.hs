{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_course (
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
version = Version [0,1,4] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/jmooyman/Library/Haskell/bin"
libdir     = "/Users/jmooyman/Library/Haskell/ghc-8.2.2-x86_64/lib/course-0.1.4"
dynlibdir  = "/Users/jmooyman/Library/Haskell/ghc-8.2.2-x86_64/lib/x86_64-osx-ghc-8.2.2"
datadir    = "/Users/jmooyman/Library/Haskell/share/ghc-8.2.2-x86_64/course-0.1.4"
libexecdir = "/Users/jmooyman/Library/Haskell/libexec/x86_64-osx-ghc-8.2.2/course-0.1.4"
sysconfdir = "/Users/jmooyman/Library/Haskell/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "course_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "course_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "course_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "course_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "course_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "course_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
