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
    , module VCSWrapper.Common.Types
) where

import VCSWrapper.Common.Types
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



