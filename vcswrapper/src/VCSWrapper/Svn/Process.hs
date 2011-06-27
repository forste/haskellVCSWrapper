-----------------------------------------------------------------------------
--
-- Module      :  Process
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

module VCSWrapper.Svn.Process (
    execute
    ,svnExec
    , module VCSWrapper.Common.Process
) where

import VCSWrapper.Common.Process
import VCSWrapper.Common.Types

{- | Execute given svn command with given options handling eventual errors and ignoring other output.
-}
execute :: String -- ^ command name, e.g. checkout
        -> [String] -- ^ options
        -> Ctx ()
execute commandName options = svnExec commandName options [] >> return ()


{- | Execute given svn command with given options and environment.
-}
svnExec :: String -- ^ svn command, e.g. checkout
        -> [String] -- ^ options
        -> [(String, String)] -- ^ environment
        -> Ctx String
svnExec cmd opts = do
    let extOpts = opts++globalOpts
    vcsExec "svn" cmd extOpts

globalOpts = ["--non-interactive"]++["--no-auth-cache"]



