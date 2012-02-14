-----------------------------------------------------------------------------
--
-- Module      :  VCSWrapper.Darcs
-- Copyright   :  2011 Stephan Fortelny, Harald Jagenteufel
-- License     :  GPL Nothing
--
-- Maintainer  :  stephanfortelny at gmail.com, h.jagenteufel at gmail.com
-- Stability   :
-- Portability :
--
-- | Wraps darcs functions with our 'Ctx' monad.
--
-----------------------------------------------------------------------------

module VCSWrapper.Darcs (
    add

    ,runVcs

    ,module VCSWrapper.Darcs.Types
) where

import VCSWrapper.Darcs.Types
import VCSWrapper.Common.VCSMonad (runVcs)

import Darcs.RunCommand (runTheCommand)
import Darcs.Commands.Help (commandControlList)

import Control.Monad (when)
import Control.Monad.Reader (liftIO, asks)
import qualified System.Directory as Directory
import Data.Maybe (fromJust, isJust)



--
-- | Tell @darcs@ to version control given files.
--
add :: [FilePath] -> Ctx ()
add paths = do
    commonOpts <- getCommonOpts
    liftIO $ runTheCommand commandControlList "add" (commonOpts ++ paths)



-- -----
-- HELPERS
-- -----

getCommonOpts :: Ctx [String]
getCommonOpts = do
    repoDir <- asks configCwd
    opts <- if (isJust repoDir) then (return ["--repodir=" ++ (fromJust repoDir)])
        else return []
    return {-"-q":-}opts
