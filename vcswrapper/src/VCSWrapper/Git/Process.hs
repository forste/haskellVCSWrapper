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
    , gitExec'
    , gitExecWithoutResult
    , module VCSWrapper.Common.Process

) where

import VCSWrapper.Common.Process
import VCSWrapper.Common.Types

import Control.Monad.Reader.Class (asks)
import qualified Control.Exception as Exc


-- | Internal function to execute a git command
gitExec :: String -- ^ git command, e.g. checkout, commit
        -> [String] -- ^ options
        -> [(String, String)] -- ^ environment
        -> Ctx String
gitExec cmd opts env = do
    cfgEnv <- asks configEnvironment
    vcsExecThrowingOnError "git" cmd opts (cfgEnv ++ env)


-- | Internal function to execute a git command. Doesn't throw an exception if the command failes,
-- but returns an Either with exit information.
gitExec' :: String -- ^ git command, e.g. checkout, commit
        -> [String] -- ^ options
        -> [(String, String)] -- ^ environment
        -> Ctx (Either VCSException String)
gitExec' cmd opts env = do
    cfgEnv <- asks configEnvironment
    vcsExec "git" cmd opts (cfgEnv ++ env)


gitExecWithoutResult :: String -- ^ git command to execute, e.g. checkout, commit
                    -> [String] -- ^ options
                    -> [(String, String)] -- ^ environment
                    -> Ctx ()
gitExecWithoutResult cmd opts env = gitExec cmd opts env >> return ()

