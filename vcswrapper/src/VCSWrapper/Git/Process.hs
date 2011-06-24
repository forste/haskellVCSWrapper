-----------------------------------------------------------------------------
--
-- Module      :  VCSWrapper.Git.Process
-- Copyright   :  Harald Jagenteufel
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module VCSWrapper.Git.Process (
    gitExec
    , gitExecWithoutResult
    , module VCSWrapper.Common.Process

) where

import VCSWrapper.Common.Process
import VCSWrapper.Common.Types

-- | Internal function to execute a git command
gitExec :: String -- ^ git command, e.g. checkout, commit
        -> [String] -- ^ options
        -> [(String, String)] -- ^ environment
        -> Ctx (Either VCSFailure String)
gitExec = vcsExec "git"

gitExecWithoutResult :: String -- ^ git command to execute, e.g. checkout, commit
                    -> [String] -- ^ options
                    -> [(String, String)] -- ^ environment
                    -> Ctx ()
gitExecWithoutResult cmd opts env = do
    o <- gitExec cmd opts env
    case o of
        Left err -> vcsError err cmd
        Right _ -> return ()
