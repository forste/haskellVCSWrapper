-----------------------------------------------------------------------------
--
-- Module      :  Common.Types
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
{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
module VCSWrapper.Common.Types (
    IsLocked
    ,LogEntry (..)
    ,Ctx(..)
    ,Config(..)
    ,Author(..)
    ,VCSFailure
    ,Status(..)
    ,Modification(..)
    ,makeConfig
    ,filePath
    ,modification
    ,isLocked
) where

import Control.Monad.Reader

data Status = SVNStatus FilePath Modification IsLocked | GITStatus FilePath Modification
--    { file :: FilePath
--    , modification :: Modification
--    , isLocked :: IsLocked
--    } | GITStatus {
--      file :: FilePath
--    , modification :: Modification
--    }
    deriving (Show,Read)

filePath :: Status -> FilePath
filePath (SVNStatus fp _ _) = fp
filePath (GITStatus fp _) = fp

modification :: Status -> Modification
modification (SVNStatus _ m _) = m
modification (GITStatus _ m) = m

isLocked :: Status -> IsLocked
isLocked (SVNStatus _ _ l) = l

data Modification = None |
                    Added |
                    Conflicting |
                    Deleted |
                    Modified |
                    Replaced |
                    Untracked |
                    Unknown |
                    Missing
    deriving (Eq,Show,Read)

data LogEntry = LogEntry {
    commitID :: String
    , author :: String
    , email :: String
    , date :: String
    , subject :: String
    , body :: String
} deriving (Show)

type IsLocked = Bool

data Config = Config
    { configCwd :: Maybe FilePath
    , configPath :: Maybe FilePath
    , configAuthor :: Maybe Author
    } deriving (Show)

data Author = Author
    { authorName :: String
    , authorEmail :: Maybe String
    } deriving (Show)

newtype Ctx a = Ctx (ReaderT Config IO a)
    deriving (Monad, MonadIO, MonadReader Config)

makeConfig :: Maybe FilePath -> Maybe FilePath -> Maybe Author -> Config
makeConfig repoPath executablePath author = Config {
        configCwd = repoPath
        ,configPath = executablePath
        ,configAuthor = author
        }

type VCSFailure = (Int,
                String,
                String,
                String,
                [String]
                )

