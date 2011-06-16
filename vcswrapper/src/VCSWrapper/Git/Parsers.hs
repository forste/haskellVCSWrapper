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
    , GitStatus (..)
    , GitLog (..)
    , LogEntry (..)
) where

import Data.List.Utils

import Text.ParserCombinators.Parsec


-- | Represents the status of a git repo as lists of files
data GitStatus = GitStatus {
    modified :: [FilePath] -- ^ modified files
    , untracked :: [FilePath] -- ^ untracked files
    , added :: [FilePath] -- ^ added files
    , removed :: [FilePath] -- ^ removed files
} deriving (Show)


-- | Holds a list of log entries
data GitLog = GitLog [LogEntry]
    deriving (Show)

data LogEntry = LogEntry {
    commitID :: String
    , author :: String
    , email :: String
    , date :: String
    , subject :: String
    , body :: String
} deriving (Show)



parseStatus :: String -> GitStatus
parseStatus status = GitStatus
        -- TODO better performance if not using list comprehension?
        [ xs | (_:x:_:xs) <- lines, x == 'M'] -- M only displayed in second column
        [ xs | (x:_:_:xs) <- lines, x == '?']
        [ xs | (x:_:_:xs) <- lines, x == 'A'] -- A only displayed in second column
        [ xs | (x:y:_:xs) <- lines, x == 'D' || y == 'D'] -- D flag depends on index state (both columns)
        where
        lines = split "\n" status

parseSimpleLog :: String -> Maybe GitLog
parseSimpleLog log =
    case parsed of
        Right entries -> Just $ GitLog entries
        Left _ -> Nothing
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
