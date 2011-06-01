-----------------------------------------------------------------------------
--
-- Module      :  GitGui.Core
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :  Harald Jagenteufel
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Lib.Git.Core (
    GitRepo
    , initRepo
--    , cloneRepo
    , openRepo
    , status
    , commit
    , checkout
    , modifiedFiles
    , untrackedFiles
    , addedFiles
    , removedFiles
) where

import System.Directory
import Control.Monad.Trans
import Data.List.Utils

import qualified Lib.Git as G
import Lib.Git.Type

--import qualified SCM.Interface as IF

newtype GitRepo = GitRepo FilePath


{- | Represents the status of a git repo as lists of:
| modified files
| untracked files
| added files
| removed files
-}
data GitStatus = GitStatus [FilePath] [FilePath] [FilePath] [FilePath] deriving (Show)

--instance IF.ScmOperations GitRepo where
--    commit                      = commit
--    checkout                    = checkout
--    getLocalPath (GitRepo path) = path
--    getModifiedFiles repo       = do
--        status <- getStatus repo
--        return $ extractModifiedFiles status



commit :: GitRepo -> [FilePath] -> String -> String -> String -> IO ()
commit _ [] _ _ _                                     = print "commit called, no files selected for commit, aborting"
commit (GitRepo repoPath) files author email message  = do
    runGit curConfig $ G.add files
    runGit curConfig $ G.commit files author email message []
    where
    curConfig = cfg repoPath

checkout :: GitRepo -> String -> IO ()
checkout (GitRepo repoPath) rev = do
    G.runGit curConfig $ G.checkout (Just rev) Nothing
    where
    curConfig = cfg repoPath


modifiedFiles :: GitStatus -> [FilePath]
modifiedFiles (GitStatus files _ _ _) = files

untrackedFiles :: GitStatus -> [FilePath]
untrackedFiles (GitStatus _ files _ _) = files

addedFiles :: GitStatus -> [FilePath]
addedFiles (GitStatus _ _ files _) = files

removedFiles :: GitStatus -> [FilePath]
removedFiles (GitStatus _ _ _ files) = files

{- | return the status of given repo as lists of:
| modified, untracked, added and removed files
-}
status :: GitRepo -> IO GitStatus
status (GitRepo path) = do
    let statusCmd = gitExec "status" ["--porcelain"] []
    rawStatus <- G.runGit (cfg path) statusCmd
    case rawStatus of
        Left err -> gitError err "status"
        Right status -> return $ parseStatus status


parseStatus :: String -> GitStatus
parseStatus status = GitStatus
        -- TODO better performance if not using list comprehension?
        [ xs | (_:x:_:xs) <- lines, x == 'M'] -- M only displayed in second column
        [ xs | (x:_:_:xs) <- lines, x == '?']
        [ xs | (x:_:_:xs) <- lines, x == 'A'] -- A only displayed in second column
        [ xs | (x:y:_:xs) <- lines, x == 'D' || y == 'D'] -- D flag depends on index state (both columns)
        where
        lines = split "\n" status


-- | initialize an empty git repository at specified FilePath
-- | TODO wrap in MaybeT ?
initRepo :: FilePath -> IO GitRepo
initRepo path = do
    G.runGit (cfg path) $ G.initDB False
    return $ GitRepo path

-- | TODO check if an initialized git repo is at specified path
-- | TODO wrap in MaybeT ?
openRepo :: FilePath -> IO GitRepo
openRepo path = do
    return $ GitRepo path


cfg :: FilePath -> Config
cfg path = G.makeConfig path Nothing
