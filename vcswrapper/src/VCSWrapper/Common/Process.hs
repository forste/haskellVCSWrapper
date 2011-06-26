-----------------------------------------------------------------------------
--
-- Module      :  VCSWrapper.Common.Process
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

module VCSWrapper.Common.Process (
    vcsExec
    ,exec
    ,runVcs
--    ,vcsError
) where

import System.Process
import System.Exit
import System.IO (Handle, hFlush, hClose, hGetContents, hPutStr)
import Control.Concurrent
import Control.Monad.Reader
import qualified Control.Exception as Exc
import VCSWrapper.Common.Types
import Data.Maybe


-- | Internal function to execute a vcs command
vcsExec :: String -- ^ VCS shell-command, e.g. git
        -> String -- ^ VCS command, e.g. checkout
        -> [String] -- ^ options
        -> [(String, String)] -- ^ environment
        -> Ctx String
vcsExec vcsName cmd opts menv  = exec cmd opts menv vcsName configPath

-- | Internal function to execute a vcs command
exec :: String -- ^ VCS command, e.g. checkout
     -> [String] -- ^ options
     -> [(String, String)] -- ^ environment
     -> String -- ^ VCS shell-command, e.g. git
     -> (Config -> Maybe FilePath) -- ^ variable getter applied to content of Ctx
     -> Ctx String
exec cmd opts menv fallBackExecutable getter = do
    cfg <- ask
    let args = cmd : opts
    let pathToExecutable = fromMaybe fallBackExecutable (getter cfg)
    (ec, out, err) <- liftIO $ readProc (configCwd cfg) pathToExecutable args menv ""
    case ec of
        ExitSuccess   -> return out
        ExitFailure i -> Exc.throw $ VCSException i out err (fromMaybe "cwd not set" $ configCwd cfg ) (cmd : opts)


{-| Run a vcs context from a config and returns the result
 -}
runVcs :: Config -> Ctx t -> IO t
runVcs config (Ctx a) = runReaderT a config

-- | Internal function to call on failure to make a friendly error message
--vcsError :: VCSFailure -> String -> b
--vcsError err msg =
--    error $ vcsErrorToString err msg

-- builds a friendly error message
--vcsErrorToString :: VCSFailure -> String -> String
--vcsErrorToString (exitval, stdout, stderr, mcwd, cmd) msg =
--    concat [ "vcs error ", "[cwd: ", mcwd,
--        "][exec: ", unwords cmd, "][exit: ", show exitval, "][msg: ", msg, "] ",
--        "stdout: ", stdout, " stderr: ", stderr ]

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
