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

withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile pattern func =
    do
       tempdir <- catch (getTemporaryDirectory) (\_ -> return ".")
       putStrLn $ "Obtained temporary directory: "++tempdir
       (file, handle) <- openTempFile tempdir pattern
       putStrLn $ "Opened file: "++file++", handle: "++show handle

       finally (func file handle)
                   (do hClose handle
                       removeFile file
                   )
