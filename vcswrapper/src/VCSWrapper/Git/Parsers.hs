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
    , parseSimpleLog
) where

import Data.List.Utils

import Text.ParserCombinators.Parsec

import VCSWrapper.Common.Types


parseStatus :: String -> [Status]
parseStatus status = [ GITStatus xs Modified | (_:x:_:xs) <- lines, x == 'M'] -- M only displayed in second column
        ++ [ GITStatus xs Untracked | (x:_:_:xs) <- lines, x == '?']
        ++ [ GITStatus xs Added | (x:_:_:xs) <- lines, x == 'A'] -- A only displayed in second column
        ++ [ GITStatus xs Deleted | (x:y:_:xs) <- lines, x == 'D' || y == 'D'] -- D flag depends on index state (both columns)
        where
        lines = split "\n" status

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
