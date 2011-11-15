-----------------------------------------------------------------------------
--
-- Module      :  Svn
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
-- TODO testing
-----------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
module VCSWrapper.Svn (
    -- svn commands
    add
    ,checkout
    ,commit
    ,lock
    ,revert
    ,resolved
    ,simpleLog
    ,unlock
    ,update
    ,status
    -- exposed svn helpers
    ,getFilesInConflict
    -- executers
    ,module VCSWrapper.Svn.Process

    --types
    ,module VCSWrapper.Svn.Types
) where

import VCSWrapper.Svn.Parsers
import VCSWrapper.Svn.Process
import VCSWrapper.Svn.Types

import VCSWrapper.Common.TemporaryFiles
import Control.Monad.Reader
import Maybe
import System.IO

import Data.List.Utils(startswith)
import Monad (filterM)
import System.Directory(doesFileExist, getDirectoryContents)
import System.FilePath(combine, splitFileName)

--
--  SVN COMMANDS
--

{- Add filepaths to repository -}
add :: [FilePath] -- files to add
        -> Maybe String -- ^ optional password
        -> [String]     -- ^ options
        -> Ctx ()
add files = svnExec_ "add" files

{- Checkout the index to some commit id creating potentially a branch -}
checkout :: [(String, Maybe String)]    -- ^ list of (url, revision), must not be empty - revision must not be set
         -> Maybe String                -- ^ path
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

{- Commit changes to the repository -}
commit :: [FilePath]  -- ^ files to commit, may be empty if not empty only specified files will be commited
         -> String       -- ^ message, must not be empty
         -> Maybe String -- ^ optional password
         -> [String]     -- ^ options
         -> Ctx ()
commit filesToCommit logMsg = svnExec_ "commit" opts
    where
        msgopts = [ "--message", logMsg ]
        opts = msgopts ++ filesToCommit

{- | Locks given files -}
lock :: [FilePath]   -- ^ files to lock, must not be empty
        -> Maybe String -- ^ optional password
        -> [String]     -- ^ additional arguments
        -> Ctx ()
lock files = svnExec_ "lock" files




{- | Revert to specific revision in current working directory -}
revert :: Integer           -- ^ revision, e.g. 3
        -> Maybe String -- ^ optional password
        -> [String]     -- ^ additional arguments
        -> Ctx()
revert revision = svnExec_ "merge" ["-rHEAD:"++show revision,"."]

{- | Remove 'conflicted' state on working copy files or directories.  -}
resolved :: [FilePath]   -- ^ files to mark resolved
        -> Maybe String -- ^ optional password
        -> [String]     -- ^ additional arguments
        -> Ctx()
resolved files = svnExec_ "resolved" files

{- | Get log from the local repository -}
simpleLog :: Ctx [LogEntry]
simpleLog = do
    o <- svnExec "log" ["--xml"] []
    logEntries <- liftIO $ withTempFile "log.xml" (parseLog o)
    return logEntries
    where
        parseLog out path handle = do
                    hPutStrLn handle out
                    hClose handle   -- closing handle so parseDocument can open one
                    parseDocument path

{- | Get status information which will be a list of (filepath, modification-status, isLocked). -}
status :: Ctx [Status]
status = do
        o <- svnExec "status" [] []
        return $ parseStatusOut o

{- | Unlocks given files -}
unlock :: [FilePath] -- ^ Files to unlock, must not be empty
          -> Maybe String -- ^ optional password
          -> [String]     -- ^ additional arguments
          -> Ctx ()
unlock files = svnExec_ "unlock" files

{- | Updates the repository -}
update :: Maybe String -- ^ optional password
       -> [String]     -- ^ additional arguments
       -> Ctx()
update = svnExec_ "update" []


--
-- Exposed SVN Helpers
--

{- | Returns all files of a conflict indicated by its associated filename.
     E.g. for file "Types.hs" this might be "Types.hs", "Types.hs.r1", "Types.hs.r2" and "Types.hs.mine" -}
getFilesInConflict :: FilePath -> Ctx [FilePath]
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




