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

-- | internal function to execute a svnadmin command
--svnadminExec :: String -> [String] -> [(String, String)]
--        -> Ctx (Either SvnFailure String)
--svnadminExec cmd opts menv = exec cmd opts menv "svnadmin" configSvnadminPath

{- | Execute given svn command with given options handling eventual errors and ignoring other output.
-}
execute :: String -- ^ command name, e.g. checkout
        -> [String] -- ^ options
        -> Ctx ()
execute commandName options = do
        o <- svnExec commandName options []
        case o of
            Right _  -> return ()
            Left err -> vcsError err commandName

{- | Execute given svn command with given options and environment.
-}
svnExec :: String -- ^ svn command, e.g. checkout
        -> [String] -- ^ options
        -> [(String, String)] -- ^ environment
        -> Ctx (Either VCSFailure String)
svnExec = vcsExec "svn"




