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

module Lib.Svn.Process where

import System.Process
import System.Exit
import System.IO (Handle, hFlush, hClose, hGetContents, hPutStr)
import Control.Concurrent
import Control.Monad.Reader
import qualified Control.Exception as C
import Lib.Svn.Types
import Data.Maybe

-- just exec with stdin/stdout/stderr as pipes
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

 -- same as readProcessWithExitCode but having a configurable cwd and env,
readProc :: Maybe FilePath --working directory or Nothing if not set
            -> String  --command
            -> [String] --arguments
            -> [(String, String)] --environment can be empty
            -> String --input can be empty
            -> IO (ExitCode, String, String)
readProc mcwd command args menv input = do
    (inh, outh, errh, pid) <- execProcWithPipes mcwd command args menv

    outMVar <- newEmptyMVar

    out <- hGetContents outh
    _ <- forkIO $ C.evaluate (length out) >> putMVar outMVar ()

    err <- hGetContents errh
    _ <- forkIO $ C.evaluate (length err) >> putMVar outMVar ()

    when (length input > 0) $ do hPutStr inh input; hFlush inh
    hClose inh

    takeMVar outMVar
    takeMVar outMVar
    hClose outh
    hClose errh

    ex <- waitForProcess pid
    return (ex, out, err)

-- | internal function to execute a svnadmin command
svnadminExec :: String -> [String] -> [(String, String)]
        -> Ctx (Either SvnFailure String)
svnadminExec cmd opts menv = exec cmd opts menv "svnadmin" configSvnadminPath

-- | internal function to execute a svn command
svnExec :: String -> [String] -> [(String, String)]
        -> Ctx (Either SvnFailure String)
svnExec cmd opts menv = exec cmd opts menv "svn" configSvnPath

exec :: String -> [String] -> [(String, String)] -> String -> (Config -> Maybe FilePath)
     -> Ctx (Either SvnFailure String)
exec cmd opts menv fallBackExecutable getter = do
    cfg <- ask
    let args = cmd : opts
    let pathToExecutable = fromMaybe fallBackExecutable (getter cfg)
    (ec, out, err) <- liftIO $ readProc (configCwd cfg) pathToExecutable args menv ""
    case ec of
        ExitSuccess   -> return $ Right out
        ExitFailure i -> return $ Left (i, out, err, fromMaybe "cwd not set" $ configCwd cfg, cmd : opts)

{-| Run a svn context from a config and returns the result
 -}
runSvn :: Config -> Ctx t -> IO t
runSvn config (Ctx a) = runReaderT a config

-- | internal function to call on failure to make a friendly error message
svnError :: SvnFailure -> String -> b
svnError err msg =
    error $ svnErrorToString err msg

--svnError :: SvnFailure -> String -> b
--svnError (exitval, stdout, stderr, mcwd, cmd) msg =
--    error $ concat [ "svn error ", "[cwd: ", mcwd,
--        "][exec: ", concat cmd, "][exit: ", show exitval, "][msg: ", msg, "] ",
--        "stdout: ", stdout, " stderr: ", stderr ]

svnErrorToString :: SvnFailure -> String -> String
svnErrorToString (exitval, stdout, stderr, mcwd, cmd) msg =
    concat [ "svn error ", "[cwd: ", mcwd,
        "][exec: ", concat cmd, "][exit: ", show exitval, "][msg: ", msg, "] ",
        "stdout: ", stdout, " stderr: ", stderr ]


