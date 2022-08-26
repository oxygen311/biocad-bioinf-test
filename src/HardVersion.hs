{-# LANGUAGE OverloadedStrings #-}

module HardVersion where

import          AminoAcidUtils
import          LightVersion

import           Bio.PDB.Type         (Atom (..), FieldType (..), PDB (..), Model(..), Chain(..))
import qualified Data.Vector     as V (Vector, concat, concatMap, map, toList)
import           Data.Graph           (Graph(..), Vertex(..), buildG, components)
import           Data.Tree            (Tree(..), flatten)
import           Linear.Metric        (distance)
import           Linear.V3            (V3(..))
import           Data.Text       as T (Text, intercalate, lines, pack,
                                       replicate, take)
import           Data.Map             (Map)
import qualified Data.Map        as M (fromList, findWithDefault)
import           Delaunay             (delaunay)
import           Voronoi3D            (voronoi3, Edge3 (..))

coordsDouble :: Atom -> [Double]
coordsDouble a = [realToFrac $ atomX a, realToFrac $ atomY a, realToFrac $ atomZ a]

isIEdge3 :: Edge3 -> Bool
isIEdge3 (IEdge3 _ ) = True
isIEdge3 _          = False

surfacePoints :: [[Double]] -> IO [[Double]]
surfacePoints ps = do
  d <- delaunay ps False False Nothing
  v <- return $ voronoi3 d
  return $ map fst $ filter (\(p, es) -> any isIEdge3 es) v