{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_myy (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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
version = Version [0,34,5] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/japiirainen/.cabal/bin"
libdir     = "/Users/japiirainen/.cabal/lib/x86_64-osx-ghc-8.10.7/myy-0.34.5-inplace-myy"
dynlibdir  = "/Users/japiirainen/.cabal/lib/x86_64-osx-ghc-8.10.7"
datadir    = "/Users/japiirainen/.cabal/share/x86_64-osx-ghc-8.10.7/myy-0.34.5"
libexecdir = "/Users/japiirainen/.cabal/libexec/x86_64-osx-ghc-8.10.7/myy-0.34.5"
sysconfdir = "/Users/japiirainen/.cabal/etc"

getBinDir     = catchIO (getEnv "myy_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "myy_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "myy_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "myy_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "myy_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "myy_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
