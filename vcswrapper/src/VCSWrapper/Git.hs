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
    , add
    , rm
    , commit
    , commitMerge
    , checkout
    , status
    , simpleLog
    , localBranches
    , revparse
    , mergetool
    , remote
    , pull
    , push
    --    , clone

    , module VCSWrapper.Git.Process -- TODO only export useful process functions

    , module VCSWrapper.Git.Types
) where

import System.Directory

import Control.Monad.Trans
import qualified Control.Exception as Exc

import Data.List.Utils

import VCSWrapper.Git.Parsers
import VCSWrapper.Git.Process
import VCSWrapper.Git.Types

import Data.Maybe
import qualified Data.List
import Data.String.Utils (strip)


exit_code_merge_conflict :: Int
exit_code_merge_conflict = 1


{- | initialize a new repository database -}
initDB :: Bool -> Ctx ()
initDB bare = do
    let opts = if bare then ["--bare"] else []
    gitExecWithoutResult "init-db" opts []

{- | add filepath to repository -}
add :: [ FilePath ] -> Ctx ()
add paths = do
    let opts = "--" : paths
    gitExecWithoutResult "add" opts []

{- | rm filepath from repository -}
rm :: [ FilePath ] -> Ctx ()
rm paths = do
    let opts = "--" : paths
    gitExecWithoutResult "rm" opts []

{- | commit change to the repository with optional filepaths and optional author + email -}
commit :: [ FilePath ] -> Maybe (String, String) -> String -> [String] -> Ctx ()
commit rsrcs mbAuthor logmsg extraopts = do
        case mbAuthor of
            Just (author, author_email) ->
                commit' rsrcs logmsg extraopts ["--author", author ++ " <" ++ author_email ++ ">"]
            Nothing ->
                commit' rsrcs logmsg extraopts []
    where
    commit' files logmsg extraopts authopts = do
        let msgopts = [ "-m", logmsg ]
        let opts = authopts ++ msgopts ++ extraopts ++ [ "--" ] ++ files
        gitExecWithoutResult "commit" opts []

{- | commit a merge using the default merge message (as in .git/MERGE_MSG) as commit message.
    Returns an error message if commit was not successfull (i.e. unmerged files still exist). -}
commitMerge :: Ctx (Either String ())
commitMerge = do
    gitRootDir <- gitExec "rev-parse" ["--show-toplevel"] [] -- returns the root directory of the current repo
    o <- gitExec' "commit" ["-F", ((strip gitRootDir) ++ "/.git/MERGE_MSG") ] []
    case o of
        Right _                             -> return $ Right ()
        Left exc@(VCSException _ out _ _ _) -> return $ Left out

{- | checkout the index to some commit id creating potentially a branch -}
checkout :: Maybe String -- ^ Commit ID
        -> Maybe String -- ^ branchname
        -> Ctx ()
checkout rev branch = do
    let bopt = maybe [] (\b -> [ "-b", b ]) branch
    let copt = maybeToList rev -- [] (: []) rev
    gitExecWithoutResult "checkout" (bopt ++ copt) []


---------------------------------
-- modification Harald Jagenteufel
---------------------------------

-- | Return the status of given repo.
status :: Ctx [Status]
status = do
    o <- gitExec "status" ["--porcelain"] []
    return $ parseStatus o

-- | get the log, maybe from a specific branch (defaults to the current branch)
simpleLog :: Maybe String -> Ctx [LogEntry]
simpleLog mbBranch = do
    -- double dash on end prevent crash if branch and filename are equal
    o <- gitExec "log" ((branch mbBranch) ++ ["--pretty=tformat:commit:%H%n%an%n%ae%n%ai%n%s%n%b%x00", "--"]) []
    return $ parseSimpleLog o
    where
    branch Nothing = []
    branch (Just b) = [b]


-- | get all local branches.
localBranches :: Ctx (String, -- ^ currently checked out branch
     [String]) -- ^ all other branches
localBranches = do
    o <- gitExec "branch" [] []
    return $ parseBranches o

-- | get all remotes
remote :: Ctx [String]
remote = do
    o <- gitExec "remote" [] []
    return $ parseRemotes o

-- | push changes to a remote
push :: Ctx ()
push = gitExecWithoutResult "push" [] []

-- | Pull changes from a remote.
-- If a merge conflict is detected, the error message is returned, otherwise Right () is returned
pull :: Ctx (Either String ())
pull = do
    o <- gitExec' "pull" [] []
    case o of
        Right _                             -> return $ Right ()
        Left exc@(VCSException _ out _ _ _) -> liftIO $ putStrLn ("caught exception: " ++ out) >>
                                            if (parsePullMergeConflict out) then return $ Left out
                                            else Exc.throw exc

-- | call git rev-parse on a given commit
revparse :: String -> Ctx (String)
revparse commit = do
    o <- gitExec "rev-parse" [commit] []
    return $ strip o

mergetool :: FilePath -> Ctx String
mergetool fp = do
    gitExec "mergetool" ["--no-prompt", fp] []




