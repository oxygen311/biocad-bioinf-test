{-# LANGUAGE OverloadedStrings #-}

module HardVersionSpec  where

import           Bio.PDB.Reader  (fromTextPDB)
import           Bio.PDB.Type    (Atom (..), FieldType (..), PDB (..))
import qualified Data.Map.Strict (empty, fromList, singleton)
import           Data.Text       as T (Text, intercalate, length, lines, pack,
                                       replicate)
import qualified Data.Vector     as V (empty, fromList, singleton)
import           Test.Hspec      (shouldBe, it, describe, Spec, shouldReturn)
import           HardVersion

surfacePointsSpec :: Spec
surfacePointsSpec = describe "Calling of surface points" $
        it "exclude point in the middle of cube" $ do
        surfacePoints [[0,0,0],[0,0,1],[0,1,0],[1,0,0],[1,1,1],[0.5,0.5,0.5]]
                      `shouldReturn` 
                      [[0,0,0],[0,0,1],[0,1,0],[1,0,0],[1,1,1]]
 