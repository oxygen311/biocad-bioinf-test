import AminoAcidUtilsSpec
import LightVersionSpec
import HardVersionSpec

import System.IO
import Test.Hspec

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hspec $ do
        pdbAtomsConcatSpec
        isHydrophobicSpec
        spaceClustersSpec
        surfacePointsSpec