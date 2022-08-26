{-# LANGUAGE OverloadedStrings #-}

module AminoAcidUtilsSpec  where

import           Bio.PDB.Reader  (fromTextPDB)
import           Bio.PDB.Type    (Atom (..), FieldType (..), PDB (..))
import qualified Data.Map.Strict (empty, fromList, singleton)
import           Data.Text       as T (Text, intercalate, length, lines, pack,
                                       replicate)
import qualified Data.Vector     as V (empty, fromList, singleton)
import           Test.Hspec      (shouldBe, it, describe, Spec, shouldSatisfy)
import           AminoAcidUtils

atomCys :: Atom
atomCys = Atom {atomSerial = 1, atomName = " OXT", atomAltLoc = ' ', atomResName = "CYS", atomChainID = 'A', atomResSeq = 214, atomICode = ' ',
                atomX = -4.546, atomY = -29.673, atomZ = 26.796, atomOccupancy = 1.0, atomTempFactor = 143.51, atomElement = " O", atomCharge = "  "}

isHydrophobicSpec :: Spec
isHydrophobicSpec = describe "Hydrophobicity test" $
        it "Cysteine should be hydrophobic" $ do
        atomCys `shouldSatisfy` isHydrophobic