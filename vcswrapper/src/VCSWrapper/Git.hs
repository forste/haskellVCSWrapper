-----------------------------------------------------------------------------
--
-- Module      :  VCSWrapper.Git
-- Copyright   :  Copyright (C) 2009-2010 Vincent Hanquez
-- License     :  AllRightsReserved
--
--   Maintainer  : Vincent Hanquez <vincent@snarc.org>, modified Harald Jagenteufel
-- Stability   :  experimental
-- Portability :
--
-- | This module provide Git functionality exec'ing the git binary.
-- give simple access to commit, checkout, status, log.
-----------------------------------------------------------------------------

module VCSWrapper.Git (
    initDB
    --    , cloneRepo
    , status
    , simpleLog
    , commit
    , checkout
    , localBranches
    , revparse

    , module VCSWrapper.Git.Process -- TODO only export useful process functions

    , module VCSWrapper.Git.Types
) where

import System.Directory
import Control.Monad.Trans
import Data.List.Utils

import VCSWrapper.Git.Parsers
import VCSWrapper.Git.Process
import VCSWrapper.Git.Types

import Data.Maybe
import qualified Data.List
import Data.String.Utils (strip)


{- | initialize a new repository database -}
initDB :: Bool -> Ctx ()
initDB bare = do
    let opts = if bare then ["--bare"] else []
    o <- gitExec "init-db" opts []
    case o of
        Right _  -> return ()
        Left err -> vcsError err "init-db"

{- | add filepath to repository -}
add :: [ FilePath ] -> Ctx ()
add paths = do
    let opts = "--" : paths
    o <- gitExec "add" opts []
    case o of
        Right _  -> return ()
        Left err -> vcsError err "add"

{- | rm filepath from repository -}
rm :: [ FilePath ] -> Ctx ()
rm paths = do
    let opts = "--" : paths
    o <- gitExec "rm" opts []
    case o of
        Right _  -> return ()
        Left err -> vcsError err "rm"

{- | commit change to the repository with optional filepaths -}
commit :: [ FilePath ] -> String -> String -> String -> [String] -> Ctx ()
commit rsrcs author author_email logmsg extraopts = do
    let authopts = [ "--author", author ++ " <" ++ author_email ++ ">" ]
    let msgopts = [ "-m", logmsg ]
    let opts = authopts ++ msgopts ++ extraopts ++ [ "--" ] ++ rsrcs
    o <- gitExec "commit" opts []
    case o of
        Right _  -> return ()
        Left err -> vcsError err "commit"

{- | checkout the index to some commit id creating potentially a branch -}
checkout :: Maybe String -- ^ Commit ID
        -> Maybe String -- ^ branchname
        -> Ctx ()
checkout rev branch = do
    let bopt = maybe [] (\b -> [ "-b", b ]) branch
    let copt = maybeToList rev -- [] (: []) rev
    _ <- gitExec "checkout" (bopt ++ copt) []
    return ()


---------------------------------
-- modification Harald Jagenteufel
---------------------------------

    -- | Return the status of given repo.
status :: Ctx [Status]
status = do
    rawStatus <- gitExec "status" ["--porcelain"] []
    case rawStatus of
        Left err -> vcsError err "status"
        Right status -> return $ parseStatus status

-- | get the log, maybe from a specific branch (defaults to the current branch)
simpleLog :: Maybe String -> Ctx [LogEntry]
simpleLog mbBranch = do
        rawLog <- gitExec "log" ((branch mbBranch) ++ ["--pretty=tformat:commit:%H%n%an%n%ae%n%ai%n%s%n%b%x00"]) []
        case rawLog of
            Left err -> vcsError err "log"
            Right log -> do
                return $ parseSimpleLog log
    where
    branch Nothing = []
    branch (Just b) = [b]


-- | get all local branches.
localBranches :: Ctx (String, -- ^ currently checked out branch
     [String]) -- ^ all other branches
localBranches = do
    rawBranches <- gitExec "branch" [] []
    case rawBranches of
        Left err -> vcsError err "branch"
        Right branches -> do
            return $ parseBranches branches


revparse :: String -> Ctx (String)
revparse commit = do
    o <- gitExec "rev-parse" [commit] []
    case o of
        Left err -> vcsError err "rev-parse"
        Right out -> return $ strip out

    -- | TODO check if an initialized git repo is at specified path
    -- TODO wrap in MaybeT ?
    --openRepo :: FilePath -- ^ .git
    --     -> String -- ^ author
    --     -> String -- ^ author email
    --     -> IO GitRepo
    --openRepo path author email = do
    --    return $ GitRepo path author email
