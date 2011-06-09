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
    add
    ,checkout
    ,commit
    ,lock
    ,unlock
    ,update
    ,execute
    ,status
    ,module VCSWrapper.Svn.Process
    ,module VCSWrapper.Svn.Types
) where


import VCSWrapper.Svn.Process
import VCSWrapper.Svn.Types
import Maybe
import Data.List.Utils

--
--  SVN COMMANDS
--

{- add filepaths to repository -}
add :: [FilePath] -- files to add
        -> [String] -- options
        -> Ctx ()
add files options= execute "add" $ files++options

{- checkout the index to some commit id creating potentially a branch -}
checkout ::  Maybe String               -- username
         -> [(String, Maybe String)]    -- list of (url, revision), must not be empty - revision must not be set
         -> Maybe String                -- path
         -> [String]                    -- options
         -> Ctx ()
checkout username repos path options = do
--    let name = "--username " ++ fromMaybe "anonymous" username ++ " " TODO comment in
    let urls = map  (\(x,y) -> x
                    ++ (if (isNothing y) then "" else "@")
                    ++ fromMaybe "" y)
                    repos
    let realPath = [fromMaybe "" path]
    execute "checkout" (options++urls++realPath)

{- commit changes to the repository -}
commit :: [FilePath]  -- files to commit, may be empty if not empty only specified files will be
                      -- commited
         -> String      -- author, must not be empty
         -> String      -- message, must not be empty
         -> [String]    -- options, may be empty
         -> Ctx ()
commit rsrcs author logmsg extraopts = do
    let authopts = [ "--username", author]
    let msgopts = [ "--message", logmsg ]
    let opts = authopts ++ msgopts ++ extraopts ++ rsrcs
    execute "commit" opts

-- create a new repository - TODO complete implementation
createRepo :: Ctx ()
createRepo = do
    o <- svnadminExec "create" [] []
    case o of
        Right _ -> return ()
        Left err -> svnError err "svnadmin create"

-- locks given files
lock :: [FilePath] -- files to lock, must not be empty
        -> String  -- lock comment, may be empty
        -> Ctx ()
lock files comment = do
        execute "lock" $ files++opts
    where
        opts = if comment==[] then [] else [ "--message", comment]

-- Get status information which will be a list of (filepath, modification-status, isLocked).
-- Options will be ignored.
status :: [String] -- options, will be ignored
         -> Ctx [SVNStatus]
status _ = do
        o <- svnExec "status" [] []
        case o of
            Right out  -> return $ parseStatusOut out
            Left err -> return $ svnError err "status"

-- unlocks given files
unlock :: [FilePath] -- files to unlock, must not be empty
          -> Ctx ()
unlock files = do
         execute "unlock" files

-- updates the repository
update :: Ctx ()
update = do
        execute "update" []


--
--  HELPERS
--

{- parses given argument. Argument is required to have same format as output from 'svn status' -}
parseStatusOut :: String -> [SVNStatus]
parseStatusOut out = parseRows rows
        where
            rows = init' splitRows
            splitRows = split "\n" out :: [String]

init' :: [a] -> [a]
init' [] = []
init' ls = init ls

{- parses given rows from 'svn status'. supports only first seven columns and filename so far -}
parseRows :: [String] -> [SVNStatus]
parseRows rows = map mapRow rows
    where
        mapRow = \row -> SVNStatus {
                                      file=(getFileName row)
                                      ,modification=(getModification row)
                                      ,isLocked=(getLockStatus row)
                                   }
        getFileName = (\row -> tail $ tail $ tail $ tail $ tail $ tail $ tail $ tail row)
                        :: String -> FilePath
        getModification = (\row -> parseFirstCol $ row!!0) :: String -> Modification
        getLockStatus = (\row -> parseSixthCol $ row!!5) :: String -> IsLocked


parseFirstCol :: Char -> Modification
parseFirstCol ' ' = None
parseFirstCol 'A' = Added
parseFirstCol 'C' = Conflicting
parseFirstCol 'D' = Deleted
parseFirstCol 'M' = Modified
parseFirstCol 'R' = Replaced
parseFirstCol '?' = Untracked
parseFirstCol _   = Unknown

parseSixthCol :: Char -> IsLocked
parseSixthCol 'K' = True
parseSixthCol ' ' = False


{- execute given svn command with given options -}
execute :: String -- command name
        -> [String] -- options
        -> Ctx ()
execute commandName options = do
        o <- svnExec commandName options []
        case o of
            Right _  -> return ()
            Left err -> svnError err commandName
