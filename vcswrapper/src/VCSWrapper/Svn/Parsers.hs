{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  VCSWrapper.Svn.Parsers
-- Copyright   :  2011 Stephan Fortelny, Harald Jagenteufel
-- License     :  GPL
--
-- Maintainer  :  stephanfortelny at gmail.com, h.jagenteufel at gmail.com
-- Stability   :
-- Portability :
--
-- | Various parsers for output of svn commands.
--
-----------------------------------------------------------------------------

module VCSWrapper.Svn.Parsers (
   parseLogFile
   ,parseStatusOut
) where
import Data.Maybe
import System.Exit
import Text.XML.HXT.Core
import Data.List.Split (splitOn)
import qualified VCSWrapper.Common.Types as Common
import Data.Text (Text)
import qualified Data.Text as T (unpack, pack, splitOn)

instance XmlPickler Log where
    xpickle = xpLog

{- |
    Attempts to parse given file and returns a list of 'Common.LogEntry'.
-}
parseLogFile :: FilePath -- ^ File which must be the same format as the one of @svn log --xml@.
              -> IO [Common.LogEntry]
parseLogFile document =
            do
            logs <- runX (xunpickleDocument xpLog [ withRemoveWS yes, withValidate no] document)
            let log = head logs
            let entries = map (\(LogEntry rev aut dat msg) -> Common.LogEntry Nothing (T.pack $ show rev) aut "" dat msg msg)
                              (logEntries log)
            return entries

{- |
    Parses given 'Text' and returns a list of 'Common.Status'.
-}
parseStatusOut :: Text -- ^ Text which is required to have same format as output from @svn status@
            -> [Common.Status]
parseStatusOut out = parseRows rows
        where
            rows = init' splitRows
            splitRows = T.splitOn "\n" out :: [Text]


--
--  HELPERS
--

-- LogFile Helpers

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

type Author = Text
type Date = Text
type Message = Text

xpLog :: PU Log
xpLog = xpWrap (\l -> Log l, \l -> (logEntries l)) $
        xpElem "log" $
        xpLogEntries

xpLogEntries :: PU LogEntries
xpLogEntries = xpList $
                xpLogEntry


xpLogEntry :: PU LogEntry
xpLogEntry =  xpWrap (\(rev,aut,dat,msg) -> LogEntry { revision = rev, author = T.pack aut, date = T.pack dat, message = T.pack msg},
                                                         \e -> (revision e, T.unpack $ author e, T.unpack $ date e, T.unpack $ message e)) $
              xpElem "logentry" $
              xp4Tuple    (xpAttr "revision" xpInt)
                          (xpElemWithText "author")
                          (xpElemWithText "date")
                          (xpElemWithText "msg")
            where
                xpElemWithText elem = xpElem elem $ xpText0

-- StatusOut Helpers

init' :: [a] -> [a]
init' [] = []
init' ls = init ls

-- parses given rows from @svn status@. Supports only first seven columns and filename so far
parseRows :: [Text] -> [Common.Status]
parseRows rows = map mapRow rows
    where
        mapRow = \row -> Common.SVNStatus (getFileName row) (getModification row)  (getLockStatus row)
--        {
--                                      file=(getFileName row)
--                                      ,modification=(getModification row)
--                                      ,isLocked=(getLockStatus row)
--                                   }
        getFileName = (\row -> nFunc tail 8 $ T.unpack row) :: Text -> FilePath
        getModification = (\row -> parseFirstCol $ T.unpack row!!0) :: Text -> Common.Modification
        getLockStatus = (\row -> parseSixthCol $ T.unpack row!!5) :: Text -> Common.IsLocked

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
