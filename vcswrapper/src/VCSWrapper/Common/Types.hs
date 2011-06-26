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
{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module VCSWrapper.Common.Types (
    VCSType(..)
    ,IsLocked
    ,LogEntry (..)
    ,Ctx(..)
    ,Config(..)
    ,Author(..)
    ,VCSException(..)
    ,Status(..)
    ,Modification(..)
    ,makeConfig
    ,makeConfigWithEnvironment
    ,filePath
    ,modification
    ,isLocked
) where

import Control.Monad.Reader
import Data.Typeable (Typeable)
import Control.Exception (Exception)

data VCSType = SVN | GIT
    deriving (Show,Read, Eq)

data Status = SVNStatus FilePath Modification IsLocked | GITStatus FilePath Modification
    deriving (Show,Read)

filePath :: Status -> FilePath
filePath (SVNStatus fp _ _) = fp
filePath (GITStatus fp _) = fp

modification :: Status -> Modification
modification (SVNStatus _ m _) = m
modification (GITStatus _ m) = m

isLocked :: Status -> IsLocked
isLocked (SVNStatus _ _ l) = l
isLocked _ = False

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
    , configEnvironment :: [(String, String)]
    } deriving (Show, Read)

data Author = Author
    { authorName :: String
    , authorEmail :: Maybe String
    } deriving (Show, Read)

newtype Ctx a = Ctx (ReaderT Config IO a)
    deriving (Monad, MonadIO, MonadReader Config)

makeConfigWithEnvironment :: Maybe FilePath -> Maybe FilePath -> Maybe Author -> [(String, String)] -> Config
makeConfigWithEnvironment repoPath executablePath author environment = Config {
        configCwd = repoPath
        ,configPath = executablePath
        ,configAuthor = author
        ,configEnvironment = environment
        }

makeConfig :: Maybe FilePath -> Maybe FilePath -> Maybe Author -> Config
makeConfig repoPath executablePath author = Config {
        configCwd = repoPath
        ,configPath = executablePath
        ,configAuthor = author
        ,configEnvironment = []
        }


data VCSException = VCSException Int String String String [String]
    deriving (Show, Typeable)

instance Exception VCSException


