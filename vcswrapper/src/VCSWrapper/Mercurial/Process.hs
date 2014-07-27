{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  VCSWrapper.Mercurial.Process
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

module VCSWrapper.Mercurial.Process (
    hgExecNoEnv
    , hgExec

    , module VCSWrapper.Common.Process
) where

import VCSWrapper.Common.Process
import VCSWrapper.Mercurial.Types
import Control.Monad.Reader(ask)
import Data.Text (Text)

hgExecNoEnv :: Text          -- ^ cmd
         -> [Text]      -- ^ opts
         -> Ctx()
hgExecNoEnv cmd options =  do
                config <- ask
                let mbAuthor  = configAuthor config
                let opts = (authopts mbAuthor)++options
                hgExec cmd opts []
                return()
                where
                authopts Nothing = []
                authopts (Just a) =  ["--username", authorName a]

{- |
    Execute given hg command with given options and environment.
-}
hgExec :: Text -- ^ hg command, e.g. checkout
        -> [Text] -- ^ options
        -> [(Text, Text)] -- ^ environment
        -> Ctx Text
hgExec cmd opts = do
    let extOpts = opts++globalOpts
    vcsExecThrowingOnError "hg" cmd extOpts
    where
        globalOpts = ["--noninteractive"]

