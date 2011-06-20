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
    ,simpleLog
    ,unlock
    ,update
    ,status

    -- executers
    ,module VCSWrapper.Svn.Process

    --types
    ,module VCSWrapper.Svn.Types
) where

import VCSWrapper.Svn.Parsers
import VCSWrapper.Common.TemporaryFiles
import VCSWrapper.Svn.Process
import VCSWrapper.Svn.Types
import Control.Monad.Reader
import Maybe
import Data.List.Utils
import System.IO
--
--  SVN COMMANDS
--

{- Add filepaths to repository -}
add :: [FilePath] -- files to add
        -> [String] -- options
        -> Ctx ()
add files options= execute "add" $ files++options

{- Checkout the index to some commit id creating potentially a branch -}
checkout :: [(String, Maybe String)]    -- ^ list of (url, revision), must not be empty - revision must not be set
         -> Maybe String                -- ^ path
         -> [String]                    -- ^ options
         -> Ctx ()
checkout repos path options = do
--    let name = "--username " ++ fromMaybe "anonymous" username ++ " " TODO comment in
    let urls = map  (\(x,y) -> x
                    ++ (if (isNothing y) then "" else "@")
                    ++ fromMaybe "" y)
                    repos
    let realPath = [fromMaybe "" path]
    execute "checkout" (options++urls++realPath)

{- Commit changes to the repository -}
commit :: [FilePath]  -- ^ files to commit, may be empty if not empty only specified files will be
                      -- ^ commited
         -> String      -- ^ author, must not be empty
         -> String      -- ^ message, must not be empty
         -> [String]    -- ^ options, may be empty
         -> Ctx ()
commit rsrcs author logmsg extraopts = do
    let authopts = [ "--username", author]
    let msgopts = [ "--message", logmsg ]
    let opts = authopts ++ msgopts ++ extraopts ++ rsrcs
    execute "commit" opts

-- create a new repository - TODO complete implementation
--createRepo :: Ctx ()
--createRepo = do
--    o <- svnadminExec "create" [] []
--    case o of
--        Right _ -> return ()
--        Left err -> svnError err "svnadmin create"

{- | Locks given files -}
lock :: [FilePath] -- files to lock, must not be empty
        -> String  -- lock comment, may be empty
        -> Ctx ()
lock files comment = do
        execute "lock" $ files++opts
    where
        opts = if comment==[] then [] else [ "--message", comment]

{- | Revert to specific revision in current working directory -}
revert :: Integer  -- ^ revision, e.g. 3
        -> Ctx()
revert revision = execute "merge" ["-rHEAD:"++show revision,"."]

{- | Get log information -}
simpleLog :: Ctx [LogEntry]
simpleLog = do
      o <- svnExec "log" ["--xml"] []
      case o of
            Right out  -> do
                        logEntries <- liftIO $ withTempFile "log.xml" (parseLog out)
                        return logEntries
            Left err -> return $ vcsError err "log"
    where
        parseLog out path handle = do
                    hPutStrLn handle out
                    hClose handle   -- closing handle so parseDocument can open one
                    parseDocument path

{- | Get status information which will be a list of (filepath, modification-status, isLocked).
   Options will be ignored. -}
status :: [String] -- ^ Options, will be ignored
         -> Ctx [Status]
status _ = do
        o <- svnExec "status" [] []
        case o of
            Right out  -> return $ parseStatusOut out
            Left err -> return $ vcsError err "status"

{- | Unlocks given files -}
unlock :: [FilePath] -- ^ Files to unlock, must not be empty
          -> Ctx ()
unlock files = do
         execute "unlock" files

{- | Updates the repository -}
update :: Ctx ()
update = do
        execute "update" []

--
--  HELPERS
--

-- parses given argument. Argument is required to have same format as output from 'svn status'
parseStatusOut :: String -> [Status]
parseStatusOut out = parseRows rows
        where
            rows = init' splitRows
            splitRows = split "\n" out :: [String]

init' :: [a] -> [a]
init' [] = []
init' ls = init ls

-- parses given rows from 'svn status'. Supports only first seven columns and filename so far
parseRows :: [String] -> [Status]
parseRows rows = map mapRow rows
    where
        mapRow = \row -> SVNStatus (getFileName row) (getModification row)  (getLockStatus row)
--        {
--                                      file=(getFileName row)
--                                      ,modification=(getModification row)
--                                      ,isLocked=(getLockStatus row)
--                                   }
        getFileName = (\row -> nFunc tail 8 row) :: String -> FilePath
        getModification = (\row -> parseFirstCol $ row!!0) :: String -> Modification
        getLockStatus = (\row -> parseSixthCol $ row!!5) :: String -> IsLocked

nFunc :: (a -> a) -> Int -> a -> a
nFunc _ 0 = id
nFunc f n = f . nFunc f (n-1)

parseFirstCol :: Char -> Modification
parseFirstCol ' ' = None
parseFirstCol 'A' = Added
parseFirstCol 'C' = Conflicting
parseFirstCol 'D' = Deleted
parseFirstCol 'M' = Modified
parseFirstCol 'R' = Replaced
parseFirstCol '?' = Untracked
parseFirstCol '!' = Missing
parseFirstCol _  = Unknown

parseSixthCol :: Char -> IsLocked
parseSixthCol 'K' = True
parseSixthCol _ = False




