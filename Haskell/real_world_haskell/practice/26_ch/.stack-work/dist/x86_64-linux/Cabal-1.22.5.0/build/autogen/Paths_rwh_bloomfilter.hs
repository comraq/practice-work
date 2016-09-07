module Paths_rwh_bloomfilter (
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
version = Version [0,1] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/adam/Documents/GitHub/ws/practice_work/Haskell/real_world_haskell/practice/26_ch/.stack-work/install/x86_64-linux/lts-6.15/7.10.3/bin"
libdir     = "/home/adam/Documents/GitHub/ws/practice_work/Haskell/real_world_haskell/practice/26_ch/.stack-work/install/x86_64-linux/lts-6.15/7.10.3/lib/x86_64-linux-ghc-7.10.3/rwh-bloomfilter-0.1-8NPN1FDCpGGBlCB0OJXzw7"
datadir    = "/home/adam/Documents/GitHub/ws/practice_work/Haskell/real_world_haskell/practice/26_ch/.stack-work/install/x86_64-linux/lts-6.15/7.10.3/share/x86_64-linux-ghc-7.10.3/rwh-bloomfilter-0.1"
libexecdir = "/home/adam/Documents/GitHub/ws/practice_work/Haskell/real_world_haskell/practice/26_ch/.stack-work/install/x86_64-linux/lts-6.15/7.10.3/libexec"
sysconfdir = "/home/adam/Documents/GitHub/ws/practice_work/Haskell/real_world_haskell/practice/26_ch/.stack-work/install/x86_64-linux/lts-6.15/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "rwh_bloomfilter_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "rwh_bloomfilter_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "rwh_bloomfilter_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "rwh_bloomfilter_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "rwh_bloomfilter_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
