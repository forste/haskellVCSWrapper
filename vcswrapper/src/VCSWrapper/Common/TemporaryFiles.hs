-----------------------------------------------------------------------------
--
-- Module      :  VCSWrapper.Common.TemporaryFiles
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

module VCSWrapper.Common.TemporaryFiles where

import System.IO
import System.Directory(getTemporaryDirectory, removeFile)
import System.IO.Error(catch)
import Control.Exception(finally)

writeToHandle :: String -> Handle -> IO()
writeToHandle tempdata handle = do
        putStrLn $ "Starting writing data to handle"
        putStrLn $ "Handle: "++show handle
        putStrLn $ "Data: "++tempdata
        hPutStrLn handle tempdata
        -- Get our new position.  This doesn't actually modify pos
       -- in memory, but makes the name "pos" correspond to a different
       -- value for the remainder of the "do" block.
        pos <- hTell handle
        putStrLn $ "After writing, my new position is " ++ show pos

        putStrLn $ "Finished writing data to handle"

withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile pattern func =
    do -- The library ref says that getTemporaryDirectory may raise on
       -- exception on systems that have no notion of a temporary directory.
       -- So, we run getTemporaryDirectory under catch.  catch takes
       -- two functions: one to run, and a different one to run if the
       -- first raised an exception.  If getTemporaryDirectory raised an
       -- exception, just use "." (the current working directory).
       tempdir <- catch (getTemporaryDirectory) (\_ -> return ".")
       putStrLn $ "Obtained temporary directory: "++tempdir
       (tempfile, temph) <- openTempFile tempdir pattern
       putStrLn $ "Opened file: "++tempfile++", handle: "++show temph


       -- Call (func tempfile temph) to perform the action on the temporary
       -- file.  finally takes two actions.  The first is the action to run.
       -- The second is an action to run after the first, regardless of
       -- whether the first action raised an exception.  This way, we ensure
       -- the temporary file is always deleted.  The return value from finally
       -- is the first action's return value.
--       func tempfile temph
--       hClose temph
--       return ()
       finally (func tempfile temph)
               (do hClose temph
                   )
