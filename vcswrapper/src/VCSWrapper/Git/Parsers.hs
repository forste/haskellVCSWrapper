-----------------------------------------------------------------------------
--
-- Module      :  VCSWrapper.Git.Parsers
-- Copyright   :  2011 Stephan Fortelny, Harald Jagenteufel
-- License     :  GPL
--
-- Maintainer  :  stephanfortelny at gmail.com, h.jagenteufel at gmail.com
-- Stability   :
-- Portability :
--
-- | Provides functions to parse the output of certain Git commands.
--
-----------------------------------------------------------------------------

module VCSWrapper.Git.Parsers (
    parseStatus
    , parseBranches
    , parseRemotes
    , parseSimpleLog
    , parsePullMergeConflict
) where

import Data.List
import Data.List.Split (splitOn)

import Text.ParserCombinators.Parsec

import VCSWrapper.Common.Types

-- | Parse the status of a Git repo. Expects command @git status --porcelain@.
parseStatus :: String -- ^ Output of @git status --porcelain@.
    -> [Status] -- ^ 'Status' for each file.
parseStatus status = [ GITStatus filepath Modified | (_:x:_:filepath) <- lines, x == 'M'] -- M only displayed in second column
        ++ [ GITStatus filepath Untracked | (x:_:_:filepath) <- lines, x == '?']
        ++ [ GITStatus filepath Added | (x:_:_:filepath) <- lines, x == 'A'] -- A only displayed in second column
        ++ [ GITStatus filepath Deleted | (x:y:_:filepath) <- lines, x == 'D' || y == 'D'] -- D flag depends on index state (both columns)
        where
        lines = splitOn "\n" status

-- | Parse the output of @git branch@.
parseBranches :: String -> (String, [String]) -- ^ (currently checked out branch, list of all other branches)
parseBranches string = (head [branchname | ('*':_:branchname) <- lined],
    [branchname | (' ':' ':branchname) <- lined])
    where
    lined = lines string

-- | Parse @git remote@.
parseRemotes :: String -> [String]
parseRemotes = splitOn "\n"

-- | Parse output of @git pull@ and return if the repository is in conflict state.
parsePullMergeConflict :: String -> Bool
parsePullMergeConflict s = isPrefixOf "CONFLICT" s -- TODO improve this implementation, conflict after attemted automerge is not detected.

-- | Parse output of @git log --pretty=tformat:commit:%H%n%an%n%ae%n%ai%n%s%n%b%x00@.
parseSimpleLog :: String -> [LogEntry]
parseSimpleLog log =
    case parsed of
        Right entries -> entries
        Left _ -> []
    where parsed = parse logEntries "" log

-- | Parse a single log entry. Expects @git log --pretty=tformat:commit:%H%n%an%n%ae%n%ai%n%s%n%b%x00@
logEntry :: Parser LogEntry
logEntry = do
    string "commit:"
    commitID <- wholeLine
    author <- wholeLine
    email <- wholeLine
    date <- wholeLine
    subject <- wholeLine
    body <- bodyLines
    char '\n'
    return $ LogEntry Nothing commitID author email date subject body

bodyLines = manyTill anyChar (nullChar)

logEntries :: Parser [LogEntry]
logEntries = manyTill logEntry eof

---------------------
-- parsing helpers
---------------------
wholeLine = manyTill anyChar newline

nullChar = satisfy (=='\0') >> return ()
