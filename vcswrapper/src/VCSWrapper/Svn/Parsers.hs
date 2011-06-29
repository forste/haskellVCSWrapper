-----------------------------------------------------------------------------
--
-- Module      :  VCSWrapper.Svn.Parsers
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module VCSWrapper.Svn.Parsers (
   parseDocument
   ,parseStatusOut
) where
import Data.Maybe
import System.Exit
import Text.XML.HXT.Core
import Data.List.Utils

import qualified VCSWrapper.Common.Types as Common

instance XmlPickler Log where
    xpickle = xpLog


parseDocument :: FilePath -> IO [Common.LogEntry]
parseDocument document =
            do
            logs <- runX (xunpickleDocument xpLog [ withRemoveWS yes, withValidate no] document)
            let log = head logs
            let entries = map (\(LogEntry rev aut dat msg) -> Common.LogEntry (show rev) aut "" dat msg msg)
                              (logEntries log)
            return entries

data Log = Log {
    logEntries :: LogEntries
    } deriving (Show, Read)

type LogEntries = [LogEntry]

data LogEntry = LogEntry {
    revision :: Int
    ,author :: Author
    , date :: Date
    , message :: Message
    } deriving (Show, Read)

type Author = String
type Date = String
type Message = String

xpLog :: PU Log
xpLog = xpWrap (\l -> Log l, \l -> (logEntries l)) $
        xpElem "log" $
        xpLogEntries

xpLogEntries :: PU LogEntries
xpLogEntries = xpList $
                xpLogEntry


xpLogEntry :: PU LogEntry
xpLogEntry =  xpWrap (\(rev,aut,dat,msg) -> LogEntry { revision = rev, author = aut, date = dat, message = msg},
                                                         \e -> (revision e, author e, date e, message e)) $
              xpElem "logentry" $
              xp4Tuple    (xpAttr "revision" xpInt)
                          (xpElemWithText "author")
                          (xpElemWithText "date")
                          (xpElemWithText "msg")
            where
                xpElemWithText elem = xpElem elem $ xpText0


--
--  HELPERS
--

-- parses given argument. Argument is required to have same format as output from 'svn status'
parseStatusOut :: String -> [Common.Status]
parseStatusOut out = parseRows rows
        where
            rows = init' splitRows
            splitRows = split "\n" out :: [String]

init' :: [a] -> [a]
init' [] = []
init' ls = init ls

-- parses given rows from 'svn status'. Supports only first seven columns and filename so far
parseRows :: [String] -> [Common.Status]
parseRows rows = map mapRow rows
    where
        mapRow = \row -> Common.SVNStatus (getFileName row) (getModification row)  (getLockStatus row)
--        {
--                                      file=(getFileName row)
--                                      ,modification=(getModification row)
--                                      ,isLocked=(getLockStatus row)
--                                   }
        getFileName = (\row -> nFunc tail 8 row) :: String -> FilePath
        getModification = (\row -> parseFirstCol $ row!!0) :: String -> Common.Modification
        getLockStatus = (\row -> parseSixthCol $ row!!5) :: String -> Common.IsLocked

nFunc :: (a -> a) -> Int -> a -> a
nFunc _ 0 = id
nFunc f n = f . nFunc f (n-1)

parseFirstCol :: Char -> Common.Modification
parseFirstCol ' ' = Common.None
parseFirstCol 'A' = Common.Added
parseFirstCol 'C' = Common.Conflicting
parseFirstCol 'D' = Common.Deleted
parseFirstCol 'M' = Common.Modified
parseFirstCol 'R' = Common.Replaced
parseFirstCol '?' = Common.Untracked
parseFirstCol '!' = Common.Missing
parseFirstCol _  =  Common.Unknown

parseSixthCol :: Char -> Common.IsLocked
parseSixthCol 'K' = True
parseSixthCol _ = False
