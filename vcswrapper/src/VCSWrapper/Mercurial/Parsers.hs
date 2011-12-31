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
) where

import VCSWrapper.Mercurial.Types
import Data.List.Utils

{- |
    Parses given 'String' and returns a list of 'Status'.
-}
parseStatusOut :: String -- ^ String which is required to have same format as output from @svn status@
            -> [Status]
parseStatusOut out = parseRows rows
        where
            rows = init' splitRows
            splitRows = split "\n" out :: [String]

-- HELPERS
init' :: [a] -> [a]
init' [] = []
init' ls = init ls

-- parses given rows from @hg status@. Supports only first seven columns and filename so far
parseRows :: [String] -> [Status]
parseRows rows = map mapRow rows
    where
        mapRow = \row -> GITStatus (getFileName row) (getModification row)
--        {
--                                      file=(getFileName row)
--                                      ,modification=(getModification row)
--                                      ,isLocked=(getLockStatus row)
--                                   }
        getFileName = (\row -> nFunc tail 2 row) :: String -> FilePath
        getModification = (\row -> parseFirstCol $ row!!0) :: String -> Modification

nFunc :: (a -> a) -> Int -> a -> a
nFunc _ 0 = id
nFunc f n = f . nFunc f (n-1)

parseFirstCol :: Char -> Modification
parseFirstCol 'C' = None
parseFirstCol 'A' = Added
parseFirstCol 'R' = Deleted
parseFirstCol 'M' = Modified
parseFirstCol '?' = Untracked
parseFirstCol '!' = Missing
parseFirstCol 'I' = Ignored
parseFirstCol _  =  Unknown
