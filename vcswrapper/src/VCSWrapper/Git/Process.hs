{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  VCSWrapper.Git.Process
-- Copyright   :  2011 Stephan Fortelny, Harald Jagenteufel
-- License     :  GPL
--
-- Maintainer  :  stephanfortelny at gmail.com, h.jagenteufel at gmail.com
-- Stability   :
-- Portability :
--
-- | Functions to execute git commands.
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
import Data.Text (Text)


-- | Internal function to execute a git command
gitExec :: Text -- ^ git command, e.g. checkout, commit
        -> [Text] -- ^ options
        -> [(Text, Text)] -- ^ environment
        -> Ctx Text
gitExec cmd opts env = do
    cfgEnv <- asks configEnvironment
    vcsExecThrowingOnError "git" cmd opts (cfgEnv ++ env)


-- | Internal function to execute a git command. Doesn't throw an exception if the command failes,
-- but returns an Either with exit information.
gitExec' :: Text -- ^ git command, e.g. checkout, commit
        -> [Text] -- ^ options
        -> [(Text, Text)] -- ^ environment
        -> Ctx (Either VCSException Text)
gitExec' cmd opts env = do
    cfgEnv <- asks configEnvironment
    vcsExec "git" cmd opts (cfgEnv ++ env)


gitExecWithoutResult :: Text -- ^ git command to execute, e.g. checkout, commit
                    -> [Text] -- ^ options
                    -> [(Text, Text)] -- ^ environment
                    -> Ctx ()
gitExecWithoutResult cmd opts env = gitExec cmd opts env >> return ()

