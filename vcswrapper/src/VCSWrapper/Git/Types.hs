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
    module VCSWrapper.Common.Types
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
