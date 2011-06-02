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
    makeConfig,
    SvnFailure,
    Ctx(..),
    Config(..)
) where

import Common.Types
import Control.Monad.Reader

makeConfig :: Maybe FilePath -> Maybe FilePath -> Maybe FilePath -> Config
makeConfig path svnPath svnadminPath = Config {
        configCwd = path,
        configSvnPath = svnPath,
        configSvnadminPath = svnadminPath
        }

type SvnFailure = (Int,
                String,
                String,
                String,
                [String]
                )

data Config = Config
    { configCwd :: Maybe FilePath
    , configSvnPath :: Maybe FilePath
    , configSvnadminPath :: Maybe FilePath
    } deriving (Show)

newtype Ctx a = Ctx (ReaderT Config IO a)
    deriving (Monad, MonadIO, MonadReader Config)
