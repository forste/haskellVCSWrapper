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
) where
import Data.Maybe
import System.Exit
import Text.XML.HXT.Core
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



