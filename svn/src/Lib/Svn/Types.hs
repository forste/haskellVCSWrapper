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
module Lib.Svn.Types (
    module Common.Types,
    Config (..),
    makeConfig,
    SvnCtx (SvnCtx),
    SvnFailure
) where

import Control.Monad.Reader
import Common.Types

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

newtype SvnCtx a = SvnCtx (ReaderT Config IO a)
    deriving (Monad, MonadIO, MonadReader Config)

type SvnFailure = (Int,
                String,
                String,
                String,
                [String]
                )
