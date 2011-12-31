-----------------------------------------------------------------------------
--
-- Module      :  VCSWrapper.Mercurial
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

module VCSWrapper.Mercurial (
    -- mercurial commands
      addremove
--    ,checkout
      ,commit
--    ,lock
--    ,mergeHeadToRevision
--    ,resolved
--    ,simpleLog
--    ,unlock
--    ,update
      ,status

      ,runVcs

      ,module VCSWrapper.Mercurial.Types
) where

import VCSWrapper.Mercurial.Process
import VCSWrapper.Mercurial.Parsers
import VCSWrapper.Mercurial.Types

import Maybe

{- |
    Add all new files, delete all missing files. Executes @hg addremove@.
-}
addremove :: [FilePath] -- ^ files to add
    -> Ctx ()
addremove files = hgExecNoEnv "addremove" files

{- |
    Update the repository's working directory to the specified changeset. If
    no changeset is specified, update to the tip of the current named branch.
    Executes @hg checkout@.
-}
checkout :: Maybe String -- ^ optional changeset
         -> [String]     -- ^ options
         -> Ctx ()
checkout mbChangeset options = hgExecNoEnv "checkout" opts
    where
        changeSet = [fromMaybe "" mbChangeset]
        opts = options++changeSet

{- |
    Commit the specified files or all outstanding changes. Executes @hg commit@.
-}
commit :: [FilePath]  -- ^ files to commit. List may be empty - if not only specified files will be commited
         -> String       -- ^ message, can be empty
         -> [String]     -- ^ options
         -> Ctx ()
commit filesToCommit logMsg options = hgExecNoEnv "commit" opts
    where
        msgOpt = [ "--message", logMsg ]
        opts = msgOpt ++ options ++ filesToCommit

{- | Show changed files in the working directory as a list of 'Status'. Executes @hg status@. -}
status :: Ctx [Status]
status = do
    o <- hgExec "status" [] []
    return $ parseStatusOut o
