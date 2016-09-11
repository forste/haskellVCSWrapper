{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  VCSWrapper.Common.Process
-- Copyright   :  2011 Stephan Fortelny, Harald Jagenteufel
-- License     :  GPL
--
-- Maintainer  :  stephanfortelny at gmail.com, h.jagenteufel at gmail.com
-- Stability   :
-- Portability :
--
-- | Functions to execute external processes.
--
-----------------------------------------------------------------------------

module VCSWrapper.Common.Process (
    vcsExec
    ,vcsExecThrowingOnError
    ,exec
) where

import System.Process
import System.Environment (getEnvironment)
import System.Exit
import System.IO (Handle, hFlush, hClose, hGetContents, hPutStr)
import Control.Concurrent
import Control.Monad.Reader (ask, liftIO, when)
import qualified Control.Exception as Exc
import VCSWrapper.Common.Types
import Data.Maybe
import Data.Monoid (mconcat)
import Data.Text (Text)
import qualified Data.Text as T (null, unpack, pack)
import qualified Data.Map.Strict as Map
import Control.Monad (unless)
import Control.Exception (IOException, try)

-- | Internal function to execute a VCS command. Throws an exception if the command fails.
vcsExecThrowingOnError :: FilePath -- ^ VCS shell-command, e.g. git
        -> Text -- ^ VCS command, e.g. checkout
        -> [Text] -- ^ options
        -> [(Text, Text)] -- ^ environment variables
        -> Ctx Text
vcsExecThrowingOnError vcsName cmd opts menv = do
    o <- vcsExec vcsName cmd opts menv
    case o of
        Right out -> return out
        Left exc  -> Exc.throw exc

-- | Internal function to execute a VCS command
vcsExec :: FilePath -- ^ VCS shell-command, e.g. git
        -> Text -- ^ VCS command, e.g. checkout
        -> [Text] -- ^ options
        -> [(Text, Text)] -- ^ environment variables
        -> Ctx (Either VCSException Text)
vcsExec vcsName cmd opts menv  = exec cmd opts menv vcsName configPath

-- | Internal function to execute a VCS command
exec :: Text -- ^ VCS command, e.g. checkout
     -> [Text] -- ^ options
     -> [(Text, Text)] -- ^ environment variables
     -> FilePath -- ^ VCS shell-command, e.g. git
     -> (Config -> Maybe FilePath) -- ^ variable getter applied to content of Ctx
     -> Ctx (Either VCSException Text)
exec cmd opts menv fallBackExecutable getter = do
    cfg <- ask
    let args = cmd : opts
    let pathToExecutable = fromMaybe fallBackExecutable (getter cfg)
    eRes <- liftIO . try $ readProc (configCwd cfg) pathToExecutable args menv ""
    let workingDir = fromMaybe "cwd not set" $ configCwd cfg
    case eRes of
        Left (exception :: IOException) -> return $ Left (VCSException 1 "" "" workingDir (cmd : opts))
        Right (ec, out, err) ->
            case ec of
                ExitSuccess   -> return $ Right out
                ExitFailure i -> return $ Left $
                    VCSException i out err workingDir (cmd : opts)

{- |
    Same as 'System.Process.readProcessWithExitCode' but having a configurable working directory and
     environment.
-}
readProc :: Maybe FilePath --working directory or Nothing if not set
            -> FilePath  --command
            -> [Text] --arguments
            -> [(Text, Text)] --environment can be empty
            -> Text --input can be empty
            -> IO (ExitCode, Text, Text)
readProc mcwd command args menv input = do
    (inh, outh, errh, pid) <- execProcWithPipes mcwd command args menv

    outMVar <- newEmptyMVar

    out <- hGetContents outh
    _ <- forkIO $ Exc.evaluate (length out) >> putMVar outMVar ()

    err <- hGetContents errh
    _ <- forkIO $ Exc.evaluate (length err) >> putMVar outMVar ()

    unless (T.null input) $ do hPutStr inh (T.unpack input); hFlush inh
    hClose inh

    takeMVar outMVar
    takeMVar outMVar
    hClose outh
    hClose errh

    ex <- waitForProcess pid
    return (ex, T.pack out, T.pack err)


{- |
    Combines the given environment variables and the environment variables of the current process, with the
    given environment variables taking priority in the case of a conflict.
-}
inheritCurrentEnvironment :: [(Text, Text)] -> IO [(String, String)]
inheritCurrentEnvironment menv = do
    currentEnv <- getEnvironment
    let menv' = map (\(k,v) -> (T.unpack k, T.unpack v)) menv
    return . Map.toList . mconcat $ map Map.fromList [menv', currentEnv]

{- |
    Setting pipes as in 'System.Process.readProcessWithExitCode' in ' but having a configurable working directory and
     environment.
-}
execProcWithPipes :: Maybe FilePath -> FilePath -> [Text] -> [(Text, Text)]
                  -> IO (Handle, Handle, Handle, ProcessHandle)
execProcWithPipes mcwd command args menv = do
    env' <- inheritCurrentEnvironment menv
    (Just inh, Just outh, Just errh, pid) <- createProcess (proc command (map T.unpack args))
        { std_in = CreatePipe,
          std_out = CreatePipe,
          std_err = CreatePipe,
          cwd = mcwd,
          env = Just env' }
    return (inh, outh, errh, pid)
