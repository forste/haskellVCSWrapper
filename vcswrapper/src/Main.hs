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

import VCSWrapper.Svn

main = do
    putStrLn "Starting!"
--    runWithConfig $ checkout (Just "hans") [(url, Nothing)] (Just targetDirectory) []
--    runWithConfig $ add ["file10"] []
--    runWithConfig $ commit [] "hansi" "commit" []
--    runWithConfig $ update
--    runWithConfig $ unlock ["file10"]
    files <- runWithConfig $ status []
    putStrLn $ "Files:"
        ++(concat $
            map (\x -> "\n"++show x)
                files)
    putStrLn "Done!"
--    runWithConfig $ add ["file5"]
--    runWithConfig $ commit ["file5"] "hansi" "first haskell commit" []

    where
        runWithConfig = runVcs curConfig
        curConfig = makeConfig (Just cwd) Nothing Nothing
        cwd = "/home/n0s/project1_work3"
        url = "file:///home/n0s/svnrep/project1/trunk"