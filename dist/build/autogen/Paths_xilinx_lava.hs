module Paths_xilinx_lava (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [5,0,1,8], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/google/home/satnam/.cabal/bin"
libdir     = "/usr/local/google/home/satnam/.cabal/lib/xilinx-lava-5.0.1.8/ghc-7.4.1"
datadir    = "/usr/local/google/home/satnam/.cabal/share/xilinx-lava-5.0.1.8"
libexecdir = "/usr/local/google/home/satnam/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "xilinx_lava_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "xilinx_lava_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "xilinx_lava_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "xilinx_lava_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
