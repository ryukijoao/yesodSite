{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_yesodSite (
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
version = Version [0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/projects/yesodSite/.stack-work/install/x86_64-linux/lts-8.12/8.0.2/bin"
libdir     = "/projects/yesodSite/.stack-work/install/x86_64-linux/lts-8.12/8.0.2/lib/x86_64-linux-ghc-8.0.2/yesodSite-0.0.0"
dynlibdir  = "/projects/yesodSite/.stack-work/install/x86_64-linux/lts-8.12/8.0.2/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/projects/yesodSite/.stack-work/install/x86_64-linux/lts-8.12/8.0.2/share/x86_64-linux-ghc-8.0.2/yesodSite-0.0.0"
libexecdir = "/projects/yesodSite/.stack-work/install/x86_64-linux/lts-8.12/8.0.2/libexec"
sysconfdir = "/projects/yesodSite/.stack-work/install/x86_64-linux/lts-8.12/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "yesodSite_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "yesodSite_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "yesodSite_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "yesodSite_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "yesodSite_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "yesodSite_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
