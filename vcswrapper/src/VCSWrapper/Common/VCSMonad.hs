-----------------------------------------------------------------------------
--
-- Module      :  VCSWrapper.Common.VCSMonad
-- Copyright   :  2011 Stephan Fortelny, Harald Jagenteufel
-- License     :  GPL
--
-- Maintainer  :  stephanfortelny at gmail.com, h.jagenteufel at gmail.com
-- Stability   :
-- Portability :
--
-- | Functions to work with the 'Ctx' monad.
--
-----------------------------------------------------------------------------

module VCSWrapper.Common.VCSMonad (
    runVcs
) where

import VCSWrapper.Common.Types (Config (..), Ctx (..))

import Control.Monad.Reader (runReaderT)

{-|
    Run a VCS 'Ctx' from a 'Config' and returns the result
 -}
runVcs :: Config -- ^ 'Config' for a VCS
       -> Ctx t -- ^ An operation running in 'Ctx'
       -> IO t
runVcs config (Ctx a) = runReaderT a config
