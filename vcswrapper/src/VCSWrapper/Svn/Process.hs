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
    svnExec
    , module VCSWrapper.Common.Process

) where

import VCSWrapper.Common.Process
import VCSWrapper.Common.Types

-- | internal function to execute a svnadmin command
--svnadminExec :: String -> [String] -> [(String, String)]
--        -> Ctx (Either SvnFailure String)
--svnadminExec cmd opts menv = exec cmd opts menv "svnadmin" configSvnadminPath

-- | Internal function to execute a svn command
svnExec :: String -- ^ svn command, e.g. checkout
        -> [String] -- ^ options
        -> [(String, String)] -- ^ environment
        -> Ctx (Either VCSFailure String)
svnExec = vcsExec "svn"



