-----------------------------------------------------------------------------
--
-- Module      :  Main
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

module Main (
    main
) where

import qualified VCSWrapper.Svn as Svn
import qualified VCSWrapper.Mercurial as Mercurial
import qualified VCSWrapper.Darcs as Darcs

import VCSWrapper.Mercurial.Parsers

--main = do
--        return()
--      parseDocument "out.xml"
--      logEntries <- runWithConfig $ simpleLog
--      putStrLn $ "LogEntries :"++show logEntries
--      stats <- runWithConfig $ status []
--      putStrLn $ "Status :"++show stats
--      runWithConfig $ commit ["file1"] "Enter commit message here." [] Nothing
--      stats <- runWithConfig $ status []
--      putStrLn $ "Status :"++show stats
--      where
--        runWithConfig = runVcs curConfig
--        curConfig = makeConfig (Just cwd) Nothing Nothing
--        cwd = "/home/n0s/project1_work4"
--        url = "file:///home/n0s/svnrep/project1/trunk"
--    putStrLn "Starting!"
----    runWithConfig $ checkout (Just "hans") [(url, Nothing)] (Just targetDirectory) []
----    runWithConfig $ add ["file10"] []
----    runWithConfig $ commit [] "hansi" "commit" []
----    runWithConfig $ update
----    runWithConfig $ unlock ["file10"]
--    files <- runWithConfig $ status []
--    putStrLn $ "Files:"
--        ++(concat $
--            map (\x -> "\n"++show x)
--                files)
--    putStrLn "Done!"
----    runWithConfig $ add ["file5"]
----    runWithConfig $ commit ["file5"] "hansi" "first haskell commit" []

cwdMercurial = "/home/forste/tmp/testvcs/hgreps/repo1"

{-
main = do
        o <- runWithConfig $ Mercurial.status
        putStrLn $ "Status:" ++ show o
        return()
        where
        runWithConfig = Mercurial.runVcs $ Mercurial.makeConfig (Just cwdMercurial) Nothing Nothing
-}

{-
main = do
        o <- runWithConfig $ Mercurial.commit ["test/test1"] "Commit via Main1" []
        putStrLn $ "Status:" ++ show o
        return()
        where
        runWithConfig = Mercurial.runVcs $ Mercurial.makeConfig (Just cwdMercurial) Nothing Nothing
-}


{-
main = do
        o <- runWithConfig $ Mercurial.simpleLog Nothing
        putStrLn $ "Log:" ++ show o
        return()
        where
        runWithConfig = Mercurial.runVcs $ Mercurial.makeConfig (Just cwdMercurial) Nothing Nothing
-}

cwdDarcs = "/home/n0s/tmp/repo1"

--{-
main = do
        runWithConfig $ Darcs.add ["file1"]
        return()
        where
        runWithConfig = Darcs.runVcs $ Darcs.makeConfig (Just cwdDarcs) Nothing Nothing
---}

{-
main = do
        o <- parseLogFile "/home/forste/tmp/testvcs/hgreps/fprog/test.log"
        putStrLn $ "Log:" ++ show o
-}
