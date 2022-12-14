{-# LANGUAGE OverloadedStrings #-}

module AminoAcidUtils where

import           Data.Text       as T (Text)
import           Bio.PDB.Type    (Atom (..))

hydropathy :: Text -> Double
hydropathy x = case x of
    "ALA" ->  1.8 
    "ARG" -> -4.5 
    "ASN" -> -3.5 
    "ASP" -> -3.5 
    "CYS" ->  2.5 
    "GLN" -> -3.5 
    "GLU" -> -3.5 
    "GLY" -> -0.4 
    "HIS" -> -3.2 
    "ILE" ->  4.5 
    "LEU" ->  3.8 
    "LYS" -> -3.9 
    "MET" ->  1.9 
    "PHE" ->  2.8 
    "PRO" -> -1.6 
    "SER" -> -0.8 
    "THR" -> -0.7 
    "TRP" -> -0.9
    "TYR" -> -1.3 
    "VAL" ->  4.2
    _   ->  0

isHydrophobic :: Atom -> Bool
isHydrophobic = (> 0) . hydropathy . atomResName