module Main (main) where

import LightVersion
import HardVersion

import Bio.PDB.Reader (fromFilePDB, PDBWarnings)
import Bio.PDB.Type   (PDB)
import Data.Either    (fromRight, fromLeft, isRight)
import Data.Text      (Text(..))
import Control.Monad  (mapM_)
import Delaunay
import Voronoi3D

processRightPDB :: ([PDBWarnings], PDB) -> IO ()
processRightPDB (wrn, pdb) = do
    let hAtomsCoords = map coordsDouble (hydrophobicAtoms pdb)
    sPoint <- surfacePoints hAtomsCoords

    mapM_ (\ps -> print ps) sPoint

main :: IO ()
main = do
    putStrLn "Enter filepath"
    filepath <- getLine
    pdb <- fromFilePDB filepath
    either print processRightPDB pdb