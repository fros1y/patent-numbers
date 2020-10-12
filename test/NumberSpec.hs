module NumberSpec where
import Prelude
import Test.Hspec
import Data.Patent.Number

spec :: Spec
spec = describe "PatentNumber" $ do
  it "generates EPODOC" $ do
    let testEPODOC = "US7654321B2"
        known = mkPatentNumber' "US" "7654321" "B2"
    toEPODOC known `shouldBe` testEPODOC