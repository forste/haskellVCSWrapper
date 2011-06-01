module Paths_common (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/n0s/.cabal/bin"
libdir     = "/home/n0s/.cabal/lib/common-0.0.1/ghc-7.0.3"
datadir    = "/home/n0s/.cabal/share/common-0.0.1"
libexecdir = "/home/n0s/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "common_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "common_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "common_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "common_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
