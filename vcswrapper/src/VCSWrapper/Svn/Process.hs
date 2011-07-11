-----------------------------------------------------------------------------
--
-- Module      :  Process
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
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

svnExec_ :: String          -- ^ cmd
         -> [String]        -- ^ cmd specific opts
         -> Maybe String -- ^ optional password
         -> [String]     -- ^ additional arguments
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
svnExecNoEnvirNoOpts :: String  -- ^ svn command, e.g. checkout
                     -> Ctx String
svnExecNoEnvirNoOpts cmd = svnExecNoEnvir cmd []


{- | Execute given svn command with given options.
-}
svnExecNoEnvir :: String    -- ^ svn command, e.g. checkout
        -> [String]         -- ^ options
        -> Ctx String
svnExecNoEnvir cmd opts = svnExec cmd opts []

{- | Execute given svn command with given options and environment.
-}
svnExec :: String -- ^ svn command, e.g. checkout
        -> [String] -- ^ options
        -> [(String, String)] -- ^ environment
        -> Ctx String
svnExec cmd opts = do
    let extOpts = opts++globalOpts
    vcsExecThrowingOnError "svn" cmd extOpts
    where
        globalOpts = ["--non-interactive"]++["--no-auth-cache"]

-- | Internal function to execute a svn command. Doesn't throw an exception if the command failes,
-- but returns an Either with exit information.
svnExec' :: String -- ^ svn command, e.g. checkout, commit
        -> [String] -- ^ options
        -> [(String, String)] -- ^ environment
        -> Ctx (Either VCSException String)
svnExec' cmd opts = do
    let extOpts = opts++globalOpts
    vcsExec "svn" cmd extOpts
    where
        globalOpts = ["--non-interactive"]++["--no-auth-cache"]







