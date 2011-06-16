-----------------------------------------------------------------------------
--
-- Module      :  VCSWrapper.Git.Types
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | All types defined and used by git
--
-----------------------------------------------------------------------------

module VCSWrapper.Git.Types (
    GitRepo (..)
    , GitStatus (..)
    , GitLog (..)
    , LogEntry (..)
) where

--import qualified SCM.Interface as IF



--instance IF.ScmOperations GitRepo where
--    commit                      = commit
--    checkout                    = checkout
--    getLocalPath (GitRepo path) = path
--    getModifiedFiles repo       = do
--        status <- getStatus repo
--        return $ extractModifiedFiles status

data GitRepo = GitRepo FilePath -- ^ path to .git
    String -- ^ Author
    String -- ^ Author email
    deriving (Show, Read)


-- | Represents the status of a git repo as lists of files
data GitStatus = GitStatus {
    modified :: [FilePath] -- ^ modified files
    , untracked :: [FilePath] -- ^ untracked files
    , added :: [FilePath] -- ^ added files
    , removed :: [FilePath] -- ^ removed files
} deriving (Show)


-- | Holds a list of log entries
data GitLog = GitLog [LogEntry]
    deriving (Show)

data LogEntry = LogEntry {
    commitID :: String
    , author :: String
    , email :: String
    , date :: String
    , subject :: String
    , body :: String
} deriving (Show)

