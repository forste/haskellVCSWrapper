-----------------------------------------------------------------------------
--
-- Module      :  Common.Types
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
{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Common.Types (
    Modification (..),
    SVNStatus (..),
    IsLocked,
    IsConflicting
) where

data SVNStatus = SVNStatus
    { file :: FilePath
    , modification :: Modification
    , isLocked :: IsLocked
    }
    deriving (Show,Read)

--instance Show SVNStatus where
--    show status = file status ++ show (modification status) ++ show (lockStatus status)


data Modification = None |
                    Added |
                    Conflicting |
                    Deleted |
                    Modified |
                    Replaced |
                    Untracked |
                    Unknown
    deriving (Eq,Show,Read)

type IsLocked = Bool
type IsConflicting = Bool
