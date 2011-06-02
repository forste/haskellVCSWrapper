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
    mapModificationToString,
    mapStringToModification,
    Modification (..)
) where

data Modification = None | Added | Deleted | Modified | Replaced | Untracked | Unknown
    deriving (Eq)

mapModificationToString :: Modification -> String
mapModificationToString modification = head $ [name | (name, mod) <- modificationAndNames, mod == modification]

mapStringToModification :: String -> Modification
mapStringToModification string = head $ [mod | (name, mod) <- modificationAndNames, name == string]


modificationAndNames :: [(String,Modification)]
modificationAndNames = [
                        ("None",None),
                        ("Added",Added),
                        ("Deleted",Deleted),
                        ("Modified",Modified),
                        ("Replaced",Replaced),
                        ("Untracked",Untracked),
                        ("Unknown",Unknown)
                        ]


