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



