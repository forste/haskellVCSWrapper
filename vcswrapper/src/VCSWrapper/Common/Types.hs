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
module VCSWrapper.Common.Types (
    Modification (..)
    ,SVNStatus (..)
    ,IsLocked
) where

data SVNStatus = SVNStatus
    { file :: FilePath
    , modification :: Modification
    , isLocked :: IsLocked
    }
    deriving (Show,Read)

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
