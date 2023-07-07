module Test.Decompose_Test where

import Search.Decompose
import Search.Search

import Test.Hspec



import Search.Search
import Algebra.PolyRings
import Algebra.Fourier
import Algebra.NTT
import Compile.PathToC
import Util.Util
import Test.Util
import Util.KernelTimer
import Util.Logger
import Compile.FAST
import Test.Hspec
import System.Random
import Control.Monad
import Compile.Compilers

import System.Environment

decomp_spec :: Spec
decomp_spec = do
  describe "decompose Paths using library" $ do

    -- Direct
    it "factor decompose" $ do -- IO
