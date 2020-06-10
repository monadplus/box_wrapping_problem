module Main (main) where

import           System.IO  (hSetEncoding, stderr, stdout, utf8)
import           Test.Hspec

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  hspec allUnitTests

allUnitTests :: Spec
allUnitTests = describe "SAT" $ do
  it "TODO" $
    error "TODO"
    --RBST.take (-1) smallRBST `shouldBe` RBST.empty
