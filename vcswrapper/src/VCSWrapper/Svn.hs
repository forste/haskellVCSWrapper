-----------------------------------------------------------------------------
--
-- Module      :  Svn
-- Copyright   :  2011 Stephan Fortelny, Harald Jagenteufel
-- License     :  GPL
--
-- Maintainer  :  stephanfortelny at gmail.com, h.jagenteufel at gmail.com
-- Stability   :
-- Portability :
--
-- | Provides high level SVN functions like @commit@, @checkout@, @update@ and others.
--
-- All functions of this module run in the 'Ctx' monad, common to all VCS.
-- On unexpected behavior, these functions will throw a 'VCSException'.
-- All functions will be executed with options @--non-interactive@ and @--no-auth-cache@ set.
-----------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
module VCSWrapper.Svn (
    -- svn commands
    add
    ,checkout
    ,commit
    ,lock
    ,mergeHeadToRevision
    ,resolved
    ,simpleLog
    ,unlock
    ,update
    ,status
    -- exposed svn helpers
    ,getFilesInConflict
    -- runner
    ,runVcs

    --types
    ,module VCSWrapper.Svn.Types
) where

import VCSWrapper.Svn.Parsers
import VCSWrapper.Svn.Process
import VCSWrapper.Svn.Types

import VCSWrapper.Common.VCSMonad (runVcs)

import VCSWrapper.Common.TemporaryFiles
import Control.Monad.Reader
import Data.Maybe
import System.IO

import Data.List.Utils(startswith)
import Control.Monad (filterM)
import System.Directory(doesFileExist, getDirectoryContents)
import System.FilePath(combine, splitFileName)

--
--  SVN COMMANDS
--

{- |
    Put files and directories under version control, scheduling them for addition to repository.
    They will be added in next commit.. Executes @svn add@.
-}
add :: [FilePath] -- ^ files to add
        -> Maybe String -- ^ optional password
        -> [String]     -- ^ options
        -> Ctx ()
add files = svnExec_ "add" files

{- |
    Checkout out a working copy from a repository. Executes @svn checkout@.
-}
checkout :: [(String, Maybe String)]    -- ^ list of (url, 'Maybe' revision). List must not be empty, however revision need not to be set
         -> Maybe String                -- ^ optional path
         -> Maybe String -- ^ optional password
         -> [String]     -- ^ options
         -> Ctx ()
checkout repos path = svnExec_ "checkout" opts
    where
        realPath = [fromMaybe "" path]
        urls =  map  (\(x,y) -> x
                    ++ (if (isNothing y) then "" else "@")
                    ++ fromMaybe "" y)
                    repos
        opts = urls++realPath

{- |
    Send changes from your working copy to the repository. Executes @svn commit@.
-}
commit :: [FilePath]  -- ^ files to commit. List may be empty - if not only specified files will be commited
         -> String       -- ^ message, can be empty
         -> Maybe String -- ^ optional password
         -> [String]     -- ^ options
         -> Ctx ()
commit filesToCommit logMsg = svnExec_ "commit" opts
    where
        msgopts = [ "--message", logMsg ]
        opts = msgopts ++ filesToCommit

{- |
    Lock working copy paths or URLs in the repository, so that no other user can commit changes to
    them. Executes @svn lock@.
-}
lock :: [FilePath]   -- ^ Files to lock, must not be empty
        -> Maybe String -- ^ optional password
        -> [String]     -- ^ options
        -> Ctx ()
lock files = svnExec_ "lock" files




{- |
    Reverts working copy to given revision. Executes @svn merge -rHEAD:$revision .@.
-}
mergeHeadToRevision :: Integer           -- ^ revision, e.g. 3
                    -> Maybe String -- ^ optional password
                    -> [String]     -- ^ options
                    -> Ctx()
mergeHeadToRevision revision = svnExec_ "merge" ["-rHEAD:"++show revision,"."]

{- |
    Remove @conflicted@ state on working copy files or directories. Executes @svn resolved@.
 -}
resolved :: [FilePath]   -- ^ files or directories to mark resolved
        -> Maybe String -- ^ optional password
        -> [String]     -- ^ options
        -> Ctx()
resolved files = svnExec_ "resolved" files

{- |
    Get the log messages for the current working copy. Executes @svn log@.
 -}
simpleLog :: Ctx [LogEntry]
simpleLog = do
    o <- svnExec "log" ["--xml"] []
    logEntries <- liftIO $ withTempFile "log.xml" (parseLog o)
    return logEntries
    where
        parseLog out path handle = do
                    hPutStrLn handle out
                    hClose handle   -- closing handle so parseDocument can open one
                    parseLogFile path

{- |
    Get the status of working copy files and directories. Executes @svn status@.
-}
status :: Ctx [Status]
status = do
        o <- svnExec "status" [] []
        return $ parseStatusOut o

{- |
    Unlock working copy paths or URLs. Executes @svn unlock@.
-}
unlock :: [FilePath] -- ^ Files to unlock, must not be empty
          -> Maybe String -- ^ optional password
          -> [String]     -- ^ options
          -> Ctx ()
unlock files = svnExec_ "unlock" files

{- |
    Bring changes from the repository into the working copy. Executes @svn update@.
-}
update :: Maybe String -- ^ optional password
       -> [String]     -- ^ options
       -> Ctx()
update = svnExec_ "update" []


--
-- Exposed SVN Helpers
--

{- |
    Returns all files of a conflict indicated by its associated filename. E.g. for file "Types.hs"
    this might be "Types.hs", "Types.hs.r1", "Types.hs.r2" and "Types.hs.mine"
-}
getFilesInConflict :: FilePath -- ^ 'FilePath' to file of conflict.
                   -> Ctx [FilePath]
getFilesInConflict fp = do
            config <- ask
            let cwd = configCwd config
            liftIO $ do
                let file = combine (fromMaybe "" cwd) fp
                let (fileD,fileN) = splitFileName file
                content <- getDirectoryContents fileD
                let contentWithD = map (\cN -> combine fileD cN) content
                files <- filterM doesFileExist contentWithD
                let filesToResolve = [f | f <- files, (startswith (file++".r") f) || (f == (file++".mine"))]++[file]
                return filesToResolve




