-----------------------------------------------------------------------------
--
-- Module      :  VCSWrapper.Git.Parsers
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

module VCSWrapper.Git.Parsers (
    parseStatus
    , parseBranches
    , parseRemotes
    , parseSimpleLog
    , parsePullMergeConflict
) where

import Data.List.Utils
import Data.List

import Text.ParserCombinators.Parsec

import VCSWrapper.Common.Types


parseStatus :: String -> [Status]
parseStatus status = [ GITStatus filepath Modified | (_:x:_:filepath) <- lines, x == 'M'] -- M only displayed in second column
        ++ [ GITStatus filepath Untracked | (x:_:_:filepath) <- lines, x == '?']
        ++ [ GITStatus filepath Added | (x:_:_:filepath) <- lines, x == 'A'] -- A only displayed in second column
        ++ [ GITStatus filepath Deleted | (x:y:_:filepath) <- lines, x == 'D' || y == 'D'] -- D flag depends on index state (both columns)
        ++ [ GITStatus filepath Conflicting | (x:y:_:filepath) <- lines, x == 'U' || y == 'U'] -- U flag depends on both columns
        where
        lines = split "\n" status

parseBranches :: String -> (String, [String])
parseBranches string = (head [branchname | ('*':_:branchname) <- lines],
    [branchname | (' ':' ':branchname) <- lines])
    where
    lines = split "\n" string

parseRemotes :: String -> [String]
parseRemotes = split "\n"

parsePullMergeConflict :: String -> Bool
parsePullMergeConflict s = isInfixOf "CONFLICT" s -- TODO maybe write more robust implementation


parseSimpleLog :: String -> [LogEntry]
parseSimpleLog log =
    case parsed of
        Right entries -> entries
        Left _ -> []
    where parsed = parse logEntries "" log

-- git log --pretty=tformat:"commit:%H%n%an%n%ae%n%ai%n%s%n%b%x00"
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
    return $ LogEntry commitID author email date subject body

bodyLines = manyTill anyChar (nullChar)

logEntries :: Parser [LogEntry]
logEntries = manyTill logEntry eof

---------------------
-- parsing helpers
---------------------
wholeLine = manyTill anyChar newline

nullChar = satisfy (=='\0') >> return ()
