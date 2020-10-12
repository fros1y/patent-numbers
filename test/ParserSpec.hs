module ParserSpec where
import Prelude
import Test.Hspec
import Data.Patent.Number
import Data.Patent.Number.Parser

spec :: Spec
spec = describe "PatentNumber" $ do
  it "parses EPODOC" $ do
    let testEPODOC = "US7654321B2"
        known = mkPatentNumber' "US" "7654321" "B2"
        parsed = parse testEPODOC
    parsed `shouldBe` Right known
  it "output EPODOC parses as EPODOC correctly" $ do
    let known = mkPatentNumber' "US" "7654321" "B2"
        epodoc = toEPODOC known
        parsed = parse epodoc
    parsed `shouldBe` Right known
  it "parses country + serial" $ do
    let test = "EP2000000"
        known = mkPatentNumber "EP" "2000000" Nothing
        parsed = parse test
    parsed `shouldBe` Right known
  it "parses docdb-style" $ do
    let test = "FR-123456-B"
        known = mkPatentNumber "FR" "123456" (Just "B")
        parsed = parse test
    parsed `shouldBe` Right known
  it "rejects bad serials" $ do
    let test = "FR-1Q23456-B"
        parsed = parse test
    parsed `shouldSatisfy` isLeft