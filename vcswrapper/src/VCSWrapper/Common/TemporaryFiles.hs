{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  VCSWrapper.Common.TemporaryFiles
-- Copyright   :  2011 Stephan Fortelny, Harald Jagenteufel
-- License     :  GPL
--
-- Maintainer  :  stephanfortelny at gmail.com, h.jagenteufel at gmail.com
-- Stability   :
-- Portability :
--
-- | Helper for temporary files.
--
-----------------------------------------------------------------------------

module VCSWrapper.Common.TemporaryFiles (
    withTempFile
) where

import System.IO
import System.Directory(getTemporaryDirectory, removeFile)
import Control.Exception as E (catch, finally, SomeException)

{- |
    Executes given function using a tempory file.
    -}
withTempFile :: FilePath -- ^ Filename
             -> (FilePath -- 'FilePath' to temporary file
                -> Handle -- 'Handle' for temporary file
                -> IO a) -- ^ Fn to be called
             -> IO a
withTempFile pattern func =
    do
       tempdir <- E.catch (getTemporaryDirectory) (\(_ :: SomeException) -> return ".")
       putStrLn $ "Obtained temporary directory: "++tempdir
       (file, handle) <- openTempFile tempdir pattern
       putStrLn $ "Opened file: "++file++", handle: "++show handle

       finally (func file handle)
                   (do hClose handle
                       removeFile file
                   )
