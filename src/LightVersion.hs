{-# LANGUAGE OverloadedStrings #-}

module LightVersion where

import          AminoAcidUtils

import           Bio.PDB.Type    (Atom (..), FieldType (..), PDB (..), Model(..), Chain(..))
import qualified Data.Vector     as V (Vector, concat, concatMap, map, toList)
import           Data.Graph           (Graph(..), Vertex(..), buildG, components)
import           Data.Tree            (Tree(..), flatten)
import           Linear.Metric        (distance)
import           Linear.V3            (V3(..))
import           Data.Text       as T (Text, intercalate, lines, pack,
                                       replicate, take)
import           Data.Map             (Map)
import qualified Data.Map        as M (fromList, findWithDefault)

coords :: Atom -> V3 Float
coords a = V3 (atomX a) (atomY a) (atomZ a)

atomCoords :: [Atom] -> Map Int (Float, Float, Float)
atomCoords atms = M.fromList (map makePair atms)
    where makePair x = (atomSerial x, (atomX x, atomY x, atomZ x))

hydrophobicAtoms :: PDB -> [Atom]
hydrophobicAtoms pdb = filter isHydrophobic $ pdbAtomsConcat pdb

pdbAtomsConcat :: PDB -> [Atom]
pdbAtomsConcat pdb = V.toList $ V.concatMap id $ V.concatMap id $ models pdb

buildAtomG :: Float -> [Atom] -> Graph
buildAtomG maxDist ats = buildG (minimum atomSerials, maximum  atomSerials) edges
  where 
    atomSerials = map atomSerial ats
    edges = [(atomSerial a1, atomSerial a2) | a1 <- ats, 
                                              a2 <- ats, 
                                              distance (coords a1) (coords a2) < maxDist]

spaceClusters :: Int -> Float -> PDB -> [[(Float, Float, Float)]]
spaceClusters sizeThreshold distThreshold pdb = connectedComponentAtomsCoordinates
  where
    connectedComponentAtoms :: [Tree Vertex]
    connectedComponentAtoms = components $ buildAtomG distThreshold $ hydrophobicAtoms pdb 

    connectedComponentAtomsFiltered :: [[Int]]
    connectedComponentAtomsFiltered = filter ((>sizeThreshold) . length) (map flatten connectedComponentAtoms)

    atomCoordinatesMap :: Map Int (Float, Float, Float)
    atomCoordinatesMap = atomCoords $ hydrophobicAtoms pdb 

    connectedComponentAtomsCoordinates :: [[(Float, Float, Float)]]
    connectedComponentAtomsCoordinates = (map.map) (\a -> M.findWithDefault (0, 0, 0) a atomCoordinatesMap) connectedComponentAtomsFiltered