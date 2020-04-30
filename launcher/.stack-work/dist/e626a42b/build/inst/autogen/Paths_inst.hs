{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_inst (
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

bindir     = "C:\\Users\\luis\\junta\\dev\\arca\\inst\\.stack-work\\install\\72fe1f5a\\bin"
libdir     = "C:\\Users\\luis\\junta\\dev\\arca\\inst\\.stack-work\\install\\72fe1f5a\\lib\\x86_64-windows-ghc-8.6.5\\inst-0.1.0.0-HfmE4asd1svC2jQn7hROW0-inst"
dynlibdir  = "C:\\Users\\luis\\junta\\dev\\arca\\inst\\.stack-work\\install\\72fe1f5a\\lib\\x86_64-windows-ghc-8.6.5"
datadir    = "C:\\Users\\luis\\junta\\dev\\arca\\inst\\.stack-work\\install\\72fe1f5a\\share\\x86_64-windows-ghc-8.6.5\\inst-0.1.0.0"
libexecdir = "C:\\Users\\luis\\junta\\dev\\arca\\inst\\.stack-work\\install\\72fe1f5a\\libexec\\x86_64-windows-ghc-8.6.5\\inst-0.1.0.0"
sysconfdir = "C:\\Users\\luis\\junta\\dev\\arca\\inst\\.stack-work\\install\\72fe1f5a\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "inst_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "inst_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "inst_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "inst_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "inst_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "inst_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
