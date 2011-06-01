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
module Lib.Svn (
    add,
    checkout,
    commit,
    execute,
    status,
    mapModificationToString,
    mapStringToModification,
    module Lib.Svn.Types,
    module Lib.Svn.Process
) where


import Lib.Svn.Process
import Lib.Svn.Types
import Maybe
import Data.List.Utils


{- create a new repository - TODO complete implementation -}
createRepo :: SvnCtx ()
createRepo = do
    o <- svnadminExec "create" [] []
    case o of
        Right _ -> return ()
        Left err -> svnError err "svnadmin create"

{- checkout the index to some commit id creating potentially a branch -}
checkout ::  Maybe String               -- username
         -> [(String, Maybe String)]    -- list of (url, revision), must not be empty - revision must not be set
         -> Maybe String                -- path
         -> [String]                    -- options
         -> SvnCtx ()
checkout username repos path options = do
--    let name = "--username " ++ fromMaybe "anonymous" username ++ " " TODO comment in
    let urls = map  (\(x,y) -> x
                    ++ (if (isNothing y) then "" else "@")
                    ++ fromMaybe "" y)
                    repos
    let realPath = [fromMaybe "" path]
    execute "checkout" (options++urls++realPath)

{- commit change to the repository with optional filepaths -}
commit :: [FilePath]  -- files to commit
         -> String      -- author
         -> String      -- message
         -> [String]    -- options, may be empty
         -> SvnCtx ()
commit rsrcs author logmsg extraopts = do
    let authopts = [ "--username", author]
    let msgopts = [ "--message", logmsg ]
    let opts = authopts ++ msgopts ++ extraopts ++ rsrcs
    execute "commit" opts

{- add filepaths to repository -}
add :: [FilePath] -- files to add
        -> [String] -- options
        -> SvnCtx ()
add files options= execute "add" $ files++options

status :: [String] -- options
         -> SvnCtx [(String, Modification)]
status options = do
        o <- svnExec "status" options []
        case o of
            Right out  -> return $ parseStatusOut out
            Left err -> return $ svnError err "status"

parseStatusOut :: String -> [(String, Modification)]
parseStatusOut out = parseRows rows
        where
            rows = init' splitRows
            splitRows = split "\n" out :: [String]

init' :: [a] -> [a]
init' [] = []
init' ls = init ls

{- supports only first seven columns and filename so far -}
parseRows :: [String] -> [(String, Modification)]
parseRows rows = map mapRow rows
    where
        mapRow = (\row -> (getFileName row, mapCharToModification $ getModification row))
        getModification = (\row -> row!!0) :: String -> Char
        getFileName = (\row -> tail $ tail $ tail $ tail $ tail $ tail $ tail $ tail row) :: String -> String


mapCharToModification :: Char -> Modification
mapCharToModification ' ' = None
mapCharToModification 'A' = Added
mapCharToModification 'D' = Deleted
mapCharToModification 'M' = Modified
mapCharToModification 'R' = Replaced
mapCharToModification '?' = Untracked
mapCharToModification _   = Unknown

mapModificationToString :: Modification -> String
mapModificationToString modification = head $ [name | (name, mod) <- modificationAndNames, mod == modification]

mapStringToModification :: String -> Modification
mapStringToModification string = head $ [mod | (name, mod) <- modificationAndNames, name == string]


modificationAndNames :: [(String,Modification)]
modificationAndNames = [
                        ("None",None),
                        ("Added",Added),
                        ("Deleted",Deleted),
                        ("Modified",Modified),
                        ("Replaced",Replaced),
                        ("Untracked",Untracked),
                        ("Unknown",Unknown)
                        ]



{- execute given svn command with given options -}
execute :: String -- command name
        -> [String] -- options
        -> SvnCtx ()
execute commandName options = do
        o <- svnExec commandName options []
        case o of
            Right _  -> return ()
            Left err -> svnError err commandName
