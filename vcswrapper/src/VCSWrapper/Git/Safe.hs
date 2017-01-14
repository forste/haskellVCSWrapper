{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  VCSWrapper.Git.Safe
-- Copyright   :  2011 Stephan Fortelny, Harald Jagenteufel
-- License     :  GPL
--
-- Maintainer  :  stephanfortelny at gmail.com, h.jagenteufel at gmail.com, hamish.k.mackenzie at gmail.com
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module VCSWrapper.Git.Safe (
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
import Data.Text (Text)
import qualified Data.Text as T (strip, pack)
import Data.Monoid ((<>))
import Control.Monad (void)


{- | Initialize a new git repository. Executes @git init@. -}
initDB :: Bool -- ^ if 'True', this repository will be initialized as a bare repository (appends @--bare@ to the git command)
    -> Ctx (Either VCSException ())
initDB bare = do
    let opts = if bare then ["--bare"] else []
    void <$> gitExec' "init-db" opts []

{- | Add files to the index. Executes @git add@. -}
add :: [ FilePath ] -- ^ 'FilePath's to add to the index.
    -> Ctx (Either VCSException ())
add paths = do
    let opts = "--" : paths
    void <$> gitExec' "add" (map T.pack opts) []

{- | Remove files from the index and the working directory. Executes @git rm@. -}
rm :: [ FilePath ] -- ^ 'FilePath's to remove.
    -> Ctx (Either VCSException ())
rm paths = do
    let opts = "--" : map T.pack paths
    void <$> gitExec' "rm" opts []

{- | Commit the current index or the specified files to the repository. Executes @git commit@. -}
commit :: [ FilePath ] -- ^ 'FilePath's to be commited instead of the current index. Leave empty to commit the index.
    -> Maybe (Text, Text) -- ^ (Author name, email)
    -> Text -- ^ Commit message. Don't leave this empty.
    -> [Text] -- ^ Options to be passed to the git executable.
    -> Ctx (Either VCSException ())
commit rsrcs mbAuthor logmsg extraopts =
        case mbAuthor of
            Just (author, author_email) ->
                commit' (map T.pack rsrcs) logmsg extraopts ["--author", author <> " <" <> author_email <> ">"]
            Nothing ->
                commit' (map T.pack rsrcs) logmsg extraopts []
    where
    commit' files logmsg extraopts authopts = do
        let msgopts = [ "-m", logmsg ]
        let opts = authopts ++ msgopts ++ extraopts ++ [ "--" ] ++ files
        void <$> gitExec' "commit" opts []

{- | Checkout the index to some commit ID. Executes @git checkout@. -}
checkout :: Maybe Text -- ^ Commit ID
        -> Maybe Text -- ^ Branchname. If specified, @git checkout -b \<branchname\>@ will be executed.
        -> Ctx (Either VCSException ())
checkout rev branch = do
    let bopt = maybe [] (\b -> [ "-b", b ]) branch
    let copt = maybeToList rev
    void <$> gitExec' "checkout" (bopt ++ copt) []

{- | Return status of the repository as a list of 'Status'. Executes @git status@. -}
status :: Ctx (Either VCSException [Status])
status = do
    e <- gitExec' "status" ["--porcelain"] []
    return (fmap parseStatus e)

{- | Get all commit messages. Executes @git log@. -}
simpleLog :: Maybe Text -- ^ The branch from which to get the commit messages. (If 'Nothing', the current branch will be used).
    -> Ctx (Either VCSException [LogEntry])
simpleLog mbBranch = do
    -- double dash on end prevent crash if branch and filename are equal
    e <- gitExec' "log" (maybeToList mbBranch ++ ["--pretty=tformat:commit:%H%n%an%n%ae%n%ai%n%s%n%b%x00", "--"]) []
    return (fmap parseSimpleLog e)

{- | Get all local branches. Executes @git branch@. -}
localBranches :: Ctx (Either VCSException (Text, [Text])) -- ^ (currently checked out branch, list of all other branches)
localBranches = do
    e <- gitExec' "branch" [] []
    return (fmap parseBranches e)

{- | Get all remotes. Executes @git remote@. -}
remote :: Ctx (Either VCSException [Text])
remote = do
    e <- gitExec' "remote" [] []
    return (fmap parseRemotes e)

{- | Push changes to the remote as configured in the git configuration. Executes @git push@. -}
push :: Ctx (Either VCSException ())
push = void <$> gitExec' "push" [] []

{- | Pull changes from the remote as configured in the git configuration. If a merge conflict
    is detected, the error message is returned, otherwise 'Right ()' is returned.
    Executes @git pull@. -}
pull :: Ctx (Either VCSException (Maybe Text))
pull = do
    o <- gitExec' "pull" [] []
    case o of
        Left exc@(VCSException _ out _ _ _) | parsePullMergeConflict out ->
            return $ Right (Just out)
        Left e -> return (Left e)
        Right _ -> return (Right Nothing)


{- | Rev-parse a revision. Executes @git rev-parse@. -}
revparse :: Text -- ^ Revision to pass to rev-parse.
    -> Ctx (Either VCSException Text)
revparse commit = do
    o <- gitExec' "rev-parse" [commit] []
    return $ fmap T.strip o






