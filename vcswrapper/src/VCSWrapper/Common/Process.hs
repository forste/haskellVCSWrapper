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
import System.Exit
import System.IO (Handle, hFlush, hClose, hGetContents, hPutStr)
import Control.Concurrent
import Control.Monad.Reader (ask, liftIO, when)
import qualified Control.Exception as Exc
import VCSWrapper.Common.Types
import Data.Maybe

-- | Internal function to execute a VCS command. Throws an exception if the command fails.
vcsExecThrowingOnError :: String -- ^ VCS shell-command, e.g. git
        -> String -- ^ VCS command, e.g. checkout
        -> [String] -- ^ options
        -> [(String, String)] -- ^ environment variables
        -> Ctx String
vcsExecThrowingOnError vcsName cmd opts menv = do
    o <- vcsExec vcsName cmd opts menv
    case o of
        Right out -> return out
        Left exc  -> Exc.throw exc

-- | Internal function to execute a VCS command
vcsExec :: String -- ^ VCS shell-command, e.g. git
        -> String -- ^ VCS command, e.g. checkout
        -> [String] -- ^ options
        -> [(String, String)] -- ^ environment variables
        -> Ctx (Either VCSException String)
vcsExec vcsName cmd opts menv  = exec cmd opts menv vcsName configPath

-- | Internal function to execute a VCS command
exec :: String -- ^ VCS command, e.g. checkout
     -> [String] -- ^ options
     -> [(String, String)] -- ^ environment variables
     -> String -- ^ VCS shell-command, e.g. git
     -> (Config -> Maybe FilePath) -- ^ variable getter applied to content of Ctx
     -> Ctx (Either VCSException String)
exec cmd opts menv fallBackExecutable getter = do
    cfg <- ask
    let args = cmd : opts
    let pathToExecutable = fromMaybe fallBackExecutable (getter cfg)
    (ec, out, err) <- liftIO $ readProc (configCwd cfg) pathToExecutable args menv ""
    case ec of
        ExitSuccess   -> return $ Right out
        ExitFailure i -> return $ Left $
            VCSException i out err (fromMaybe "cwd not set" $ configCwd cfg ) (cmd : opts)

{- |
    Same as 'System.Process.readProcessWithExitCode' but having a configurable working directory and
     environment.
-}
readProc :: Maybe FilePath --working directory or Nothing if not set
            -> String  --command
            -> [String] --arguments
            -> [(String, String)] --environment can be empty
            -> String --input can be empty
            -> IO (ExitCode, String, String)
readProc mcwd command args menv input = do
    putStrLn $ "Executing process, mcwd: "++show mcwd++", command: "++command++
                ",args: "++show args++",menv: "++show menv++", input"++input
    (inh, outh, errh, pid) <- execProcWithPipes mcwd command args menv

    outMVar <- newEmptyMVar

    out <- hGetContents outh
    _ <- forkIO $ Exc.evaluate (length out) >> putMVar outMVar ()

    err <- hGetContents errh
    _ <- forkIO $ Exc.evaluate (length err) >> putMVar outMVar ()

    when (length input > 0) $ do hPutStr inh input; hFlush inh
    hClose inh

    takeMVar outMVar
    takeMVar outMVar
    hClose outh
    hClose errh

    ex <- waitForProcess pid
    return (ex, out, err)


{- |
    Setting pipes as in 'System.Process.readProcessWithExitCode' in ' but having a configurable working directory and
     environment.
-}
execProcWithPipes :: Maybe FilePath -> String -> [String] -> [(String, String)]
                  -> IO (Handle, Handle, Handle, ProcessHandle)
execProcWithPipes mcwd command args menv = do
    (Just inh, Just outh, Just errh, pid) <- createProcess (proc command args)
        { std_in = CreatePipe,
          std_out = CreatePipe,
          std_err = CreatePipe,
          cwd = mcwd,
          env = Just menv }
    return (inh, outh, errh, pid)
