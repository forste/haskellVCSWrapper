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
    Modification (..)
    ,SVNStatus (..)
    ,IsLocked
    ,LogEntry (..)
    ,Ctx(..)
    ,Config(..)
    ,VCSFailure
    ,makeConfig
) where

import Control.Monad.Reader

data SVNStatus = SVNStatus
    { file :: FilePath
    , modification :: Modification
    , isLocked :: IsLocked
    }
    deriving (Show,Read)

data Modification = None |
                    Added |
                    Conflicting |
                    Deleted |
                    Modified |
                    Replaced |
                    Untracked |
                    Unknown
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
    } deriving (Show)

newtype Ctx a = Ctx (ReaderT Config IO a)
    deriving (Monad, MonadIO, MonadReader Config)

makeConfig :: Maybe FilePath -> Maybe FilePath -> Maybe FilePath -> Config
makeConfig path svnPath svnadminPath = Config {
        configCwd = path
        ,configPath = svnPath
        }

type VCSFailure = (Int,
                String,
                String,
                String,
                [String]
                )

