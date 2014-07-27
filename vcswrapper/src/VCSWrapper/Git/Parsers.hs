{-# LANGUAGE OverloadedStrings #-}
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
import Data.Text (Text)
import qualified Data.Text as T
       (isPrefixOf, pack, lines, unpack, splitOn)
import Control.Applicative ((<$>))

-- | Parse the status of a Git repo. Expects command @git status --porcelain@.
parseStatus :: Text -- ^ Output of @git status --porcelain@.
    -> [Status] -- ^ 'Status' for each file.
parseStatus status = [ GITStatus filepath Modified | (_:x:_:filepath) <- lines, x == 'M'] -- M only displayed in second column
        ++ [ GITStatus filepath Untracked | (x:_:_:filepath) <- lines, x == '?']
        ++ [ GITStatus filepath Added | (x:_:_:filepath) <- lines, x == 'A'] -- A only displayed in second column
        ++ [ GITStatus filepath Deleted | (x:y:_:filepath) <- lines, x == 'D' || y == 'D'] -- D flag depends on index state (both columns)
        where
        lines = map T.unpack $ T.splitOn "\n" status

-- | Parse the output of @git branch@.
parseBranches :: Text -> (Text, [Text]) -- ^ (currently checked out branch, list of all other branches)
parseBranches string = (head [T.pack branchname | ('*':_:branchname) <- lined],
    [T.pack branchname | (' ':' ':branchname) <- lined])
    where
    lined = map T.unpack $ T.lines string

-- | Parse @git remote@.
parseRemotes :: Text -> [Text]
parseRemotes = T.splitOn "\n"

-- | Parse output of @git pull@ and return if the repository is in conflict state.
parsePullMergeConflict :: Text -> Bool
parsePullMergeConflict s = T.isPrefixOf "CONFLICT" s -- TODO improve this implementation, conflict after attemted automerge is not detected.

-- | Parse output of @git log --pretty=tformat:commit:%H%n%an%n%ae%n%ai%n%s%n%b%x00@.
parseSimpleLog :: Text -> [LogEntry]
parseSimpleLog log =
    case parsed of
        Right entries -> entries
        Left _ -> []
    where parsed = parse logEntries "" (T.unpack log)

-- | Parse a single log entry. Expects @git log --pretty=tformat:commit:%H%n%an%n%ae%n%ai%n%s%n%b%x00@
logEntry :: Parser LogEntry
logEntry = do
    string "commit:"
    commitID <- T.pack <$> wholeLine
    author <- T.pack <$> wholeLine
    email <- T.pack <$> wholeLine
    date <- T.pack <$> wholeLine
    subject <- T.pack <$> wholeLine
    body <- T.pack <$> bodyLines
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
