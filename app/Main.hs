module Main (main) where

import LightVersion

import Bio.PDB.Reader (fromFilePDB, PDBWarnings)
import Bio.PDB.Type   (PDB)
import Data.Either    (fromRight, fromLeft, isRight)
import Data.Text      (Text(..))
import Control.Monad  (mapM_)

maximumDistAtoms :: Float
maximumDistAtoms = 3.0

minimalClusterSizeatoms :: Int
minimalClusterSizeatoms = 10

processRightPDB :: ([PDBWarnings], PDB) -> IO ()
processRightPDB (wrn, pdb) = mapM_ (\ls -> do {putStrLn "_____________"; print ls}) 
                                   (spaceClusters minimalClusterSizeatoms maximumDistAtoms pdb)

main :: IO ()
main = do
    putStrLn "Enter filepath"
    filepath <- getLine
    pdb <- fromFilePDB filepath
    either print processRightPDB pdb