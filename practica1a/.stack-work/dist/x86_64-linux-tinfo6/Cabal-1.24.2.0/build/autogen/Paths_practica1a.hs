{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_practica1a (
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

bindir     = "/home/danielavignau/Documents/universidad-autonoma-de-madrid/aprog/lab/ampliacion-de-programacion/practica1a/.stack-work/install/x86_64-linux-tinfo6/7d72947fdcf54dc9ee3013b26bc31c6c0beec84586fdea149a93159589afea93/8.0.2/bin"
libdir     = "/home/danielavignau/Documents/universidad-autonoma-de-madrid/aprog/lab/ampliacion-de-programacion/practica1a/.stack-work/install/x86_64-linux-tinfo6/7d72947fdcf54dc9ee3013b26bc31c6c0beec84586fdea149a93159589afea93/8.0.2/lib/x86_64-linux-ghc-8.0.2/practica1a-0.1.0.0-2NmO6uoRgWnGqLh6Jwansc"
dynlibdir  = "/home/danielavignau/Documents/universidad-autonoma-de-madrid/aprog/lab/ampliacion-de-programacion/practica1a/.stack-work/install/x86_64-linux-tinfo6/7d72947fdcf54dc9ee3013b26bc31c6c0beec84586fdea149a93159589afea93/8.0.2/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/danielavignau/Documents/universidad-autonoma-de-madrid/aprog/lab/ampliacion-de-programacion/practica1a/.stack-work/install/x86_64-linux-tinfo6/7d72947fdcf54dc9ee3013b26bc31c6c0beec84586fdea149a93159589afea93/8.0.2/share/x86_64-linux-ghc-8.0.2/practica1a-0.1.0.0"
libexecdir = "/home/danielavignau/Documents/universidad-autonoma-de-madrid/aprog/lab/ampliacion-de-programacion/practica1a/.stack-work/install/x86_64-linux-tinfo6/7d72947fdcf54dc9ee3013b26bc31c6c0beec84586fdea149a93159589afea93/8.0.2/libexec"
sysconfdir = "/home/danielavignau/Documents/universidad-autonoma-de-madrid/aprog/lab/ampliacion-de-programacion/practica1a/.stack-work/install/x86_64-linux-tinfo6/7d72947fdcf54dc9ee3013b26bc31c6c0beec84586fdea149a93159589afea93/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "practica1a_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "practica1a_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "practica1a_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "practica1a_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "practica1a_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "practica1a_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
