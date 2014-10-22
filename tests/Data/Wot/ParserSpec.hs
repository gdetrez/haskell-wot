module Data.Wot.ParserSpec where
-- import Data.Monoid ((<>))
-- import Data.ByteString.Lazy (ByteString)
-- import qualified Data.ByteString.Lazy as B

import Test.Hspec
-- import Test.QuickCheck

import Data.Wot.Parser
import Data.ByteString.Lazy (pack, ByteString)
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Base16.Lazy (decode)
import Control.Exception (evaluate)

import Paths_wot (getDataFileName)

spec :: Spec
spec = do

    describe "pNames" $ do
      it "returns an empty list when given an empty content" $
        pNames "" `shouldBe` []
      it "Splits the list of names on new lines" $ do
        let myData = "abc\ndef ghi\n"
        pNames myData `shouldBe` ["abc", "def ghi"]

    describe "key" $ do
        it "create a key given a bytestring of the right length" $
            key "56e2" `shouldBe` Key "56e2"

        it "fails if the key doesn't have the right length" $
            evaluate (key "56e") `shouldThrow` anyException

    describe "hex" $ do
        it "reads a bytestring given as hexadecimal string" $
            hex "56e2f643" `shouldBe` pack [0x56, 0xe2, 0xf6, 0x43]

    describe "pKeys" $ do
      it "returns an empty list when given an empty content" $
        pKeys "" `shouldBe` []
      it "returns a list of keys" $ do
          let myData = pack [0x56, 0xe2, 0xf6, 0x43, 0xa0, 0x5f, 0x79, 0xbf]
          pKeys myData `shouldBe` [key (hex "56e2f643"), key (hex "a05f79bf")]

    describe "pSignatures" $ do
      it "returns an empty list when given an empty bytestring" $
          pSignatures "" `shouldBe` []

      it "correctly parses one set of signatures" $
          pSignatures (hex "00000002600004be70007f3e")
            `shouldBe` [[S (True, 2, 1214), S (True, 3, 32574)]]

    describe "pSig" $ do
      it "parses a single signature" $
          pSig 0x600004be `shouldBe` S (True, 2, 1214)

    describe "readWot" $ do
      it "parses a downloaded wot file" $ do
          file <- getDataFileName "2014-09-21.wot" >>= B.readFile
          let result = readWot file
          length result `shouldBe` 55246 -- number from the 'debug' section



hex :: ByteString -> ByteString
hex b = case decode b of
    (b',e) | B.null e -> b'
    (_,_)             -> error "readHex: invalid hex sequence"
