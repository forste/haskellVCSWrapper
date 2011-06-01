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
    SvnFailure
) where

import Common.Types

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
