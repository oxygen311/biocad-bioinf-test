{-# LANGUAGE OverloadedStrings #-}

module LightVersionSpec  where

import           Bio.PDB.Reader  (fromTextPDB)
import           Bio.PDB.Type    (Atom (..), FieldType (..), PDB (..))
import qualified Data.Map.Strict (empty, fromList, singleton)
import           Data.Text       as T (Text, intercalate, length, lines, pack,
                                       replicate)
import qualified Data.Vector     as V (empty, fromList, singleton)
import           Test.Hspec      (shouldBe, it, describe, Spec )
import           LightVersion

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

someAtomsList :: [Atom]
someAtomsList = [Atom {atomSerial = 1, atomName = " OXT", atomAltLoc = ' ', atomResName = "CYS", atomChainID = 'A', atomResSeq = 214, atomICode = ' ',
                        atomX = -4.546, atomY = -29.673, atomZ = 26.796, atomOccupancy = 1.0, atomTempFactor = 143.51, atomElement = " O", atomCharge = "  "},
                Atom {atomSerial = 2, atomName = " H  ", atomAltLoc = ' ', atomResName = "CYS", atomChainID = 'A', atomResSeq = 214, atomICode = ' ', 
                        atomX = -6.124, atomY = -27.225, atomZ = 26.558, atomOccupancy = 1.0, atomTempFactor = 15.0, atomElement = " H", atomCharge = "  "},
                Atom {atomSerial = 3, atomName = " N  ", atomAltLoc = ' ', atomResName = "GLU", atomChainID = 'B', atomResSeq = 1, atomICode = ' ',
                        atomX = 18.637, atomY = -61.583, atomZ = 66.852, atomOccupancy = 1.0, atomTempFactor = 118.48, atomElement = " N", atomCharge = "  "},
                Atom {atomSerial = 4, atomName = " C1 ", atomAltLoc = ' ', atomResName = "NAG", atomChainID = 'B', atomResSeq = 475, atomICode = ' ',
                        atomX = 5.791, atomY = -20.194, atomZ = -7.051, atomOccupancy = 1.0, atomTempFactor = 34.66, atomElement = " C", atomCharge = "  "},
                Atom {atomSerial = 5, atomName = " C4 ", atomAltLoc = ' ', atomResName = "NAG", atomChainID = 'B', atomResSeq = 475, atomICode = ' ', 
                        atomX = 6.943, atomY = -19.507, atomZ = -9.597, atomOccupancy = 1.0, atomTempFactor = 25.87, atomElement = " C", atomCharge = "  "},
                Atom {atomSerial = 6, atomName = " CA ", atomAltLoc = ' ', atomResName = "GLU", atomChainID = 'B', atomResSeq = 1, atomICode = ' ',
                        atomX = 19.722, atomY = -62.606, atomZ = 66.868, atomOccupancy = 1.0, atomTempFactor = 19.77, atomElement = " C", atomCharge = "  "},
                Atom {atomSerial = 7, atomName = " CA ", atomAltLoc = ' ', atomResName = "GLU", atomChainID = 'B', atomResSeq = 1, atomICode = 'A',
                        atomX = 19.722, atomY = -62.606, atomZ = 66.868, atomOccupancy = 1.0, atomTempFactor = 19.77, atomElement = " C", atomCharge = "  "}]


oneModelPDB :: PDB
oneModelPDB =  PDB { title = "STRUCTURE OF THE TRANSFORMED MONOCLINIC  LYSOZYME"
                   , remarks = Data.Map.Strict.singleton (Just 1) (V.singleton "REFERENCE 1")
                   , models = V.singleton $ V.fromList [V.fromList someAtomsHead,
                                                        V.fromList someAtomsTail
                                                        ]
                  , otherFields = Data.Map.Strict.fromList [(HEADER, V.fromList [" header"]),(COMPND,V.fromList[" compnd"]),(SOURCE,V.fromList[" source"]),(KEYWDS,V.fromList[" keywds"]),(AUTHOR,V.fromList["  Masha"]),(REVDAT,V.fromList[" revdat"]),(SEQRES,V.fromList [" seqres"]),
                                                            (CRYST1,V.fromList [" cryst1"]),(ORIGX1,V.fromList[" origx1 n=1"]),(SCALE2,V.fromList [" sclaen n=2"]),(MASTER,V.fromList[" 1 2 3 4 5 6 7 8"])]
                   }
                   where someAtomsHead = slice 0 2 someAtomsList
                         someAtomsTail = slice 3 7 someAtomsList

pdbAtomsConcatSpec :: Spec
pdbAtomsConcatSpec = describe "Simple concat of Atoms to vector" $
        it "concat atoms correctly" $ do
        pdbAtomsConcat oneModelPDB `shouldBe` someAtomsList

spaceClustersSpec :: Spec
spaceClustersSpec = describe "Calling of space clusters based on hydrophobic atoms" $
        it "call atoms correctly" $ do
        spaceClusters 1 3.0 oneModelPDB `shouldBe` [[(-4.546,-29.673,26.796),(-6.124,-27.225,26.558)]]
 