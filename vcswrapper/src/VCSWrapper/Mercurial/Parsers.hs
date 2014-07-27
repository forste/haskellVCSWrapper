{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
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
    ,parseBranches
) where

import qualified VCSWrapper.Mercurial.Types as Common
import Text.XML.HXT.Core
import Data.List.Split (splitOn)
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack, lines, splitOn)
import Control.Applicative ((<$>))

{- |
    Parses given 'Text' and returns a list of 'Status'.
-}
parseStatusOut :: Text -- ^ Text which is required to have same format as output from @svn status@
            -> [Common.Status]
parseStatusOut out = parseRows rows
        where
            rows = init' splitRows
            splitRows = T.splitOn "\n" out :: [Text]

-- | Parse the output of @hg branch -q@.
parseBranches :: Text -> [Text] -- ^ list of all branches
parseBranches string = T.lines string

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
            let entries = map (\(LogEntry rev node mbBranch (email, aut) dat msg) -> Common.LogEntry mbBranch node aut email dat msg msg)
                              (logEntries log)
            return entries

-- HELPERS
init' :: [a] -> [a]
init' [] = []
init' ls = init ls

-- parses given rows from @hg status@. Supports only first seven columns and filename so far
parseRows :: [Text] -> [Common.Status]
parseRows rows = map mapRow rows
    where
        mapRow = \row -> Common.GITStatus (getFileName row) (getModification row)
--        {
--                                      file=(getFileName row)
--                                      ,modification=(getModification row)
--                                      ,isLocked=(getLockStatus row)
--                                   }
        getFileName = (\row -> nFunc tail 2 $ T.unpack row) :: Text -> FilePath
        getModification = (\row -> parseFirstCol $ T.unpack row!!0) :: Text -> Common.Modification



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
    ,node :: Text
    ,branch :: Maybe Text
    ,author :: Author
    , date :: Date
    , message :: Message
    } deriving (Show, Read)

type Author = (Text, Text)
type Date = Text
type Message = Text

xpLog :: PU Log
xpLog = xpWrap (\l -> Log l, \l -> (logEntries l))
            $ xpElem "log"
            $ xpLogEntries

xpLogEntries :: PU LogEntries
xpLogEntries = xpList
                $ xpLogEntry


xpLogEntry :: PU LogEntry
xpLogEntry =  xpWrap (\(rev,nod,bra,par,tag,aut,dat,msg) ->
                        LogEntry { revision = rev, node = T.pack nod, branch = T.pack <$> bra, author = aut, date = T.pack dat, message = T.pack msg},
                      \e -> (revision e, T.unpack $ node e, T.unpack <$> branch e, Nothing, Nothing, author e, T.unpack $ date e, T.unpack $ message e))
                $ xpElem "logentry"
                $ xp8Tuple (xpAttr "revision" xpInt)
                           (xpAttr "node" xpText0)
                           (xpOption $ xpElemWithText "branch")
                           (xpOption $ xpElemWithText "parent")
                           (xpOption $ xpElemWithText "tag")
                           (xpAuthor)
                           (xpElemWithText "date")
                           (xpElemWithText "msg")
                where
                xpElemWithText elem = xpElem elem $ xpText0


xpAuthor :: PU Author
xpAuthor = xpWrap (packPair, unpackPair)
            $ xpElem "author"
            $ xpPair (xpAttr "email" xpText0)
                     (xpText0)
    where
        packPair (a, b) = (T.pack a, T.pack b)
        unpackPair (a, b) = (T.unpack a, T.unpack b)
