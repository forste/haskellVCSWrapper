module Paths_svntest (
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
libdir     = "/home/n0s/.cabal/lib/svntest-0.0.1/ghc-7.0.3"
datadir    = "/home/n0s/.cabal/share/svntest-0.0.1"
libexecdir = "/home/n0s/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "svntest_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "svntest_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "svntest_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "svntest_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
