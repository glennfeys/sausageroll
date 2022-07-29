{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_sausage (
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

bindir     = "C:\\Users\\Luc Feys\\Desktop\\project functioneel\\projecten-2018-2019\\sausage-roll\\.stack-work\\install\\db7ce97c\\bin"
libdir     = "C:\\Users\\Luc Feys\\Desktop\\project functioneel\\projecten-2018-2019\\sausage-roll\\.stack-work\\install\\db7ce97c\\lib\\x86_64-windows-ghc-8.4.3\\sausage-0.1.0.0-38qQhvIP65Q5V3SnAsHmxk-sausage-roll"
dynlibdir  = "C:\\Users\\Luc Feys\\Desktop\\project functioneel\\projecten-2018-2019\\sausage-roll\\.stack-work\\install\\db7ce97c\\lib\\x86_64-windows-ghc-8.4.3"
datadir    = "C:\\Users\\Luc Feys\\Desktop\\project functioneel\\projecten-2018-2019\\sausage-roll\\.stack-work\\install\\db7ce97c\\share\\x86_64-windows-ghc-8.4.3\\sausage-0.1.0.0"
libexecdir = "C:\\Users\\Luc Feys\\Desktop\\project functioneel\\projecten-2018-2019\\sausage-roll\\.stack-work\\install\\db7ce97c\\libexec\\x86_64-windows-ghc-8.4.3\\sausage-0.1.0.0"
sysconfdir = "C:\\Users\\Luc Feys\\Desktop\\project functioneel\\projecten-2018-2019\\sausage-roll\\.stack-work\\install\\db7ce97c\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "sausage_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "sausage_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "sausage_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "sausage_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "sausage_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "sausage_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
