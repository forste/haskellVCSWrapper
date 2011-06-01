-----------------------------------------------------------------------------
--
-- Module      :  Types
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
module Lib.Svn.Types where

import Control.Monad.Reader

data Config = Config
    { configCwd :: Maybe FilePath
    , configSvnPath :: Maybe FilePath
    , configSvnadminPath :: Maybe FilePath
    } deriving (Show)

makeConfig :: Maybe FilePath -> Maybe FilePath -> Maybe FilePath -> Config
makeConfig path svnPath svnadminPath = Config {
        configCwd = path,
        configSvnPath = svnPath,
        configSvnadminPath = svnadminPath
        }

data Modification = None | Added | Deleted | Modified | Replaced | Untracked | Unknown
    deriving (Eq)

newtype SvnCtx a = SvnCtx (ReaderT Config IO a)
    deriving (Monad, MonadIO, MonadReader Config)

type SvnFailure = (Int,
                String,
                String,
                String,
                [String]
                )
