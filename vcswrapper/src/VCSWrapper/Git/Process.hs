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
import Control.Monad.Reader.Class (asks)


-- | Internal function to execute a git command
gitExec :: String -- ^ git command, e.g. checkout, commit
        -> [String] -- ^ options
        -> [(String, String)] -- ^ environment
        -> Ctx String
gitExec cmd opts env = do
    cfgEnv <- asks configEnvironment
    vcsExec "git" cmd opts (cfgEnv ++ env)

gitExecWithoutResult :: String -- ^ git command to execute, e.g. checkout, commit
                    -> [String] -- ^ options
                    -> [(String, String)] -- ^ environment
                    -> Ctx ()
gitExecWithoutResult cmd opts env = gitExec cmd opts env >> return ()
