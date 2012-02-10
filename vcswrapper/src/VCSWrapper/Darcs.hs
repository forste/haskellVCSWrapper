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
    setupDarcs
    liftIO $ runTheCommand commandControlList "add" paths


setupDarcs :: Ctx ()
setupDarcs = do
    -- we have to cd to the repo directory
    repoDir <- asks configCwd
    when (isJust repoDir) (liftIO $ Directory.setCurrentDirectory $ fromJust repoDir)
