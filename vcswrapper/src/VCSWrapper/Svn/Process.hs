{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  Process
-- Copyright   :  2011 Stephan Fortelny, Harald Jagenteufel
-- License     :  GPL
--
-- Maintainer  :  stephanfortelny at gmail.com, h.jagenteufel at gmail.com
-- Stability   :
-- Portability :
--
-- | Functions to execute svn commands.
--
-----------------------------------------------------------------------------

module VCSWrapper.Svn.Process (
    svnExec
    , svnExec_
    , svnExec'
    , svnExecNoEnvir
    , svnExecNoEnvirNoOpts

    , module VCSWrapper.Common.Process
) where

import VCSWrapper.Common.Process
import VCSWrapper.Common.Types

import Control.Monad.Reader(ask)

import qualified Control.Exception as Exc
import Data.Text (Text)

svnExec_ :: Text          -- ^ cmd
         -> [Text]        -- ^ cmd specific opts
         -> Maybe Text -- ^ optional password
         -> [Text]     -- ^ additional arguments
         -> Ctx()
svnExec_ cmd cmdOpts pw opts =  do
                config <- ask
                let mbAuthor  = configAuthor config
                let builtOpts = (pwopts pw)++(authopts mbAuthor)++opts
                svnExecNoEnvir cmd $ builtOpts ++ cmdOpts
                return()
    where
    useropts Nothing  = []
    useropts (Just u) = ["--username",u]
    pwopts Nothing  = []
    pwopts (Just p) = ["--password",p]
    authopts Nothing = []
    authopts (Just a) =  ["--username", authorName a]
{- | Execute given svn command with given options.
-}
svnExecNoEnvirNoOpts :: Text  -- ^ svn command, e.g. checkout
                     -> Ctx Text
svnExecNoEnvirNoOpts cmd = svnExecNoEnvir cmd []


{- | Execute given svn command with given options.
-}
svnExecNoEnvir :: Text    -- ^ svn command, e.g. checkout
        -> [Text]         -- ^ options
        -> Ctx Text
svnExecNoEnvir cmd opts = svnExec cmd opts []

{- | Execute given svn command with given options and environment.
-}
svnExec :: Text -- ^ svn command, e.g. checkout
        -> [Text] -- ^ options
        -> [(Text, Text)] -- ^ environment
        -> Ctx Text
svnExec cmd opts = do
    let extOpts = opts++globalOpts
    vcsExecThrowingOnError "svn" cmd extOpts
    where
        globalOpts = ["--non-interactive"]++["--no-auth-cache"]

-- | Internal function to execute a svn command. Doesn't throw an exception if the command failes,
-- but returns an Either with exit information.
svnExec' :: Text -- ^ svn command, e.g. checkout, commit
        -> [Text] -- ^ options
        -> [(Text, Text)] -- ^ environment
        -> Ctx (Either VCSException Text)
svnExec' cmd opts = do
    let extOpts = opts++globalOpts
    vcsExec "svn" cmd extOpts
    where
        globalOpts = ["--non-interactive"]++["--no-auth-cache"]







