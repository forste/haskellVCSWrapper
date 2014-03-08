-----------------------------------------------------------------------------
--
-- Module      :  VCSWrapper.Git
-- Copyright   :  2011 Stephan Fortelny, Harald Jagenteufel
-- License     :  GPL
--
-- Maintainer  :  stephanfortelny at gmail.com, h.jagenteufel at gmail.com
-- Stability   :
-- Portability :
--
-- | Provides high-level Git functions like @commit@, @checkout@, @status@, @log@,...
--
-- All functions of this module run in the 'Ctx' monad, common to all VCS.
-- On unexpected behavior, these functions will throw a 'VCSException'.
-----------------------------------------------------------------------------

module VCSWrapper.Git (
    initDB
    , add
    , rm
    , commit
    , checkout
    , status
    , simpleLog
    , localBranches
    , revparse
    , remote
    , pull
    , push
    --    , clone

    -- reexport from VCSWrapper.Git.Process
    , runVcs

    , module VCSWrapper.Git.Types
) where

import System.Directory

import Control.Monad.Trans
import qualified Control.Exception as Exc

import VCSWrapper.Git.Parsers
import VCSWrapper.Git.Process
import VCSWrapper.Git.Types

import VCSWrapper.Common.VCSMonad (runVcs)

import Data.Maybe
import qualified Data.List
import Data.Text (strip, pack, unpack)


{- | Initialize a new git repository. Executes @git init@. -}
initDB :: Bool -- ^ if 'True', this repository will be initialized as a bare repository (appends @--bare@ to the git command)
    -> Ctx ()
initDB bare = do
    let opts = if bare then ["--bare"] else []
    gitExecWithoutResult "init-db" opts []

{- | Add files to the index. Executes @git add@. -}
add :: [ FilePath ] -- ^ 'FilePath's to add to the index.
    -> Ctx ()
add paths = do
    let opts = "--" : paths
    gitExecWithoutResult "add" opts []

{- | Remove files from the index and the working directory. Executes @git rm@. -}
rm :: [ FilePath ] -- ^ 'FilePath's to remove.
    -> Ctx ()
rm paths = do
    let opts = "--" : paths
    gitExecWithoutResult "rm" opts []

{- | Commit the current index or the specified files to the repository. Executes @git commit@. -}
commit :: [ FilePath ] -- ^ 'FilePath's to be commited instead of the current index. Leave empty to commit the index.
    -> Maybe (String, String) -- ^ (Author name, email)
    -> String -- ^ Commit message. Don't leave this empty.
    -> [String] -- ^ Options to be passed to the git executable.
    -> Ctx ()
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

{- | Checkout the index to some commit ID. Executes @git checkout@. -}
checkout :: Maybe String -- ^ Commit ID
        -> Maybe String -- ^ Branchname. If specified, @git checkout -b \<branchname\>@ will be executed.
        -> Ctx ()
checkout rev branch = do
    let bopt = maybe [] (\b -> [ "-b", b ]) branch
    let copt = maybeToList rev
    gitExecWithoutResult "checkout" (bopt ++ copt) []

{- | Return status of the repository as a list of 'Status'. Executes @git status@. -}
status :: Ctx [Status]
status = do
    o <- gitExec "status" ["--porcelain"] []
    return $ parseStatus o

{- | Get all commit messages. Executes @git log@. -}
simpleLog :: Maybe String -- ^ The branch from which to get the commit messages. (If 'Nothing', the current branch will be used).
    -> Ctx [LogEntry]
simpleLog mbBranch = do
    -- double dash on end prevent crash if branch and filename are equal
    o <- gitExec "log" ((branch mbBranch) ++ ["--pretty=tformat:commit:%H%n%an%n%ae%n%ai%n%s%n%b%x00", "--"]) []
    return $ parseSimpleLog o
    where
    branch Nothing = []
    branch (Just b) = [b]


{- | Get all local branches. Executes @git branch@. -}
localBranches :: Ctx (String, [String]) -- ^ (currently checked out branch, list of all other branches)
localBranches = do
    o <- gitExec "branch" [] []
    return $ parseBranches o

{- | Get all remotes. Executes @git remote@. -}
remote :: Ctx [String]
remote = do
    o <- gitExec "remote" [] []
    return $ parseRemotes o

{- | Push changes to the remote as configured in the git configuration. Executes @git push@. -}
push :: Ctx ()
push = gitExecWithoutResult "push" [] []

{- | Pull changes from the remote as configured in the git configuration. If a merge conflict
    is detected, the error message is returned, otherwise 'Right ()' is returned.
    Executes @git pull@. -}
pull :: Ctx (Either String ())
pull = do
    o <- gitExec' "pull" [] []
    case o of
        Right _                             -> return $ Right ()
        Left exc@(VCSException _ out _ _ _) -> if (parsePullMergeConflict out) then return $ Left out
                                            else Exc.throw exc

{- | Rev-parse a revision. Executes @git rev-parse@. -}
revparse :: String -- ^ Revision to pass to rev-parse.
    -> Ctx (String)
revparse commit = do
    o <- gitExec "rev-parse" [commit] []
    return . unpack . strip $ pack o






