-----------------------------------------------------------------------------
--
-- Module      :  VCSWrapper.Mercurial.Parsers
-- Copyright   :  2011 Stephan Fortelny, Harald Jagenteufel
-- License     :  GPL Nothing
--
-- Maintainer  :  stephanfortelny at gmail.com, h.jagenteufel at gmail.com
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module VCSWrapper.Mercurial.Parsers (
    parseStatusOut
    ,parseLogFile
) where

import qualified VCSWrapper.Mercurial.Types as Common
import Data.List.Utils
import Text.XML.HXT.Core


{- |
    Parses given 'String' and returns a list of 'Status'.
-}
parseStatusOut :: String -- ^ String which is required to have same format as output from @svn status@
            -> [Common.Status]
parseStatusOut out = parseRows rows
        where
            rows = init' splitRows
            splitRows = split "\n" out :: [String]

{- |
    Attempts to parse given file and returns a list of 'Common.LogEntry'.
-}
parseLogFile :: FilePath -- ^ File which must be the same format as the one of @hg log --style xml@.
              -> IO [Common.LogEntry]
parseLogFile document =
            do
            logs <- runX (xunpickleDocument xpLog [ withRemoveWS yes, withValidate no] document)
            putStrLn $ "Logs: " ++ show logs
            let log = head logs
            let entries = map (\(LogEntry rev node (email, aut) dat msg) -> Common.LogEntry node aut email dat msg msg)
                              (logEntries log)
            return entries

-- HELPERS
init' :: [a] -> [a]
init' [] = []
init' ls = init ls

-- parses given rows from @hg status@. Supports only first seven columns and filename so far
parseRows :: [String] -> [Common.Status]
parseRows rows = map mapRow rows
    where
        mapRow = \row -> Common.GITStatus (getFileName row) (getModification row)
--        {
--                                      file=(getFileName row)
--                                      ,modification=(getModification row)
--                                      ,isLocked=(getLockStatus row)
--                                   }
        getFileName = (\row -> nFunc tail 2 row) :: String -> FilePath
        getModification = (\row -> parseFirstCol $ row!!0) :: String -> Common.Modification



nFunc :: (a -> a) -> Int -> a -> a
nFunc _ 0 = id
nFunc f n = f . nFunc f (n-1)

parseFirstCol :: Char -> Common.Modification
parseFirstCol 'C' = Common.None
parseFirstCol 'A' = Common.Added
parseFirstCol 'R' = Common.Deleted
parseFirstCol 'M' = Common.Modified
parseFirstCol '?' = Common.Untracked
parseFirstCol '!' = Common.Missing
parseFirstCol 'I' = Common.Ignored
parseFirstCol _  =  Common.Unknown

-- LogFile Helpers

data Log = Log {
    logEntries :: LogEntries
    } deriving (Show, Read)

instance XmlPickler Log where
    xpickle = xpLog

type LogEntries = [LogEntry]

data LogEntry = LogEntry {
    revision :: Int
    ,node :: String
    ,author :: Author
    , date :: Date
    , message :: Message
    } deriving (Show, Read)

type Author = (String, String)
type Date = String
type Message = String

xpLog :: PU Log
xpLog = xpWrap (\l -> Log l, \l -> (logEntries l))
            $ xpElem "log"
            $ xpLogEntries

xpLogEntries :: PU LogEntries
xpLogEntries = xpList
                $ xpLogEntry


xpLogEntry :: PU LogEntry
xpLogEntry =  xpWrap (\(_,rev,nod,aut,dat,msg) ->
                        LogEntry { revision = rev, node = nod, author = aut, date = dat, message = msg},
                      \e -> (Nothing, revision e, node e, author e, date e, message e))
                $ xpElem "logentry"
                $ xp6Tuple (xpOption $ xpElemWithText "tag")
                           (xpAttr "revision" xpInt)
                           (xpAttr "node" xpText0)
                           (xpAuthor)
                           (xpElemWithText "date")
                           (xpElemWithText "msg")
                where
                xpElemWithText elem = xpElem elem $ xpText0


xpAuthor :: PU Author
xpAuthor = xpWrap (id, id)
            $ xpElem "author"
            $ xpPair (xpAttr "email" xpText0)
                     (xpText0)

