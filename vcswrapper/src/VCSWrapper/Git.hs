-----------------------------------------------------------------------------
--
-- Module      :  VCSWrapper.Git
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :  Harald Jagenteufel
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module VCSWrapper.Git (
--    GitRepo (..) -- TODO remove this constructor again, somehow...
--    , initRepo
----    , cloneRepo
--    , openRepo
--    , status
--    , simpleLog
--    , commit
--    , checkout
--    , modifiedFiles
--    , untrackedFiles
--    , addedFiles
--    , removedFiles
--    , GitStatus
--    , GitLog (..)
--    , LogEntry (..)
) where

import System.Directory
import Control.Monad.Trans
import Data.List.Utils

import qualified Lib.Git as G
import Lib.Git.Type

import VCSWrapper.Git.Parsers
import VCSWrapper.Git.Types



--commit :: GitRepo -> [FilePath] -> String -> IO ()
--commit _ [] _                                     = print "commit called, no files selected for commit, aborting"
--commit (GitRepo repoPath author email) files message  = do
--    runGit curConfig $ G.add files
--    runGit curConfig $ G.commit files author email message []
--    where
--    curConfig = cfg repoPath
--
--checkout :: GitRepo -> String -> IO ()
--checkout (GitRepo repoPath _ _) rev = do
--    G.runGit curConfig $ G.checkout (Just rev) Nothing
--    where
--    curConfig = cfg repoPath
--
--
--modifiedFiles :: GitStatus -> [FilePath]
--modifiedFiles (GitStatus files _ _ _) = files
--
--untrackedFiles :: GitStatus -> [FilePath]
--untrackedFiles (GitStatus _ files _ _) = files
--
--addedFiles :: GitStatus -> [FilePath]
--addedFiles (GitStatus _ _ files _) = files
--
--removedFiles :: GitStatus -> [FilePath]
--removedFiles (GitStatus _ _ _ files) = files
--
--{- | return the status of given repo as lists of:
--   | modified, untracked, added and removed files
---}
--status :: GitRepo -> IO GitStatus
--status (GitRepo path _ _) = do
--    let statusCmd = gitExec "status" ["--porcelain"] []
--    rawStatus <- G.runGit (cfg path) statusCmd
--    case rawStatus of
--        Left err -> gitError err "status"
--        Right status -> return $ parseStatus status
--
--
--simpleLog :: GitRepo -> IO (Maybe GitLog)
--simpleLog (GitRepo path _ _) = do
--    let logCmd = gitExec "log" ["--pretty=tformat:commit:%H%n%an%n%ae%n%ai%n%s%n%b%x00"] []
--    rawLog <- G.runGit (cfg path) logCmd
--    case rawLog of
--        Left err -> gitError err "log"
--        Right log -> do
----            liftIO $ putStrLn log
--            return $ parseSimpleLog log
--
--
--
---- | initialize an empty git repository at specified FilePath
---- | TODO wrap in MaybeT ?
--initRepo :: FilePath -- ^ .git
--     -> String -- ^ author
--     -> String -- ^ author email
--     -> IO GitRepo
--initRepo path author email = do
--    G.runGit (cfg path) $ G.initDB False
--    return $ GitRepo path author email
--
---- | TODO check if an initialized git repo is at specified path
---- | TODO wrap in MaybeT ?
--openRepo :: FilePath -- ^ .git
--     -> String -- ^ author
--     -> String -- ^ author email
--     -> IO GitRepo
--openRepo path author email = do
--    return $ GitRepo path author email
--
--
--cfg :: FilePath -> VCSWrapper.Git.Types.Config
--cfg path = G.makeConfig path Nothing
