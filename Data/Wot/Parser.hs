module Data.Wot.Parser where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Bits
import Data.Word
import Data.Binary.Get
import Control.Applicative ((<$>),(<*>))
import Control.Monad (replicateM, liftM)
import Codec.Compression.BZip (decompress)
import Codec.Archive.Ar (readAr)

readWot :: L.ByteString -> [(Key, L.ByteString, [Signature])]
readWot bs = keys
  where
    files = readAr (decompress bs)
    ("names",      _, _, _, _, namesBytes)      = files !! 2
    ("keys",       _, _, _, _, keysBytes)       = files !! 3
    ("signatures", _, _, _, _, signaturesBytes) = files !! 4
    names = pNames namesBytes
    keyIds = pKeys keysBytes
    sigs = pSignatures signaturesBytes
    keys = zip3 keyIds names sigs


pNames :: L.ByteString -> [L.ByteString]
pNames = BC.lines

newtype Key = Key L.ByteString
  deriving (Show,Eq)

key :: L.ByteString -> Key
key k | L.length k == 4 = Key k
key _ = error "key: Invalid key length"

pKeys :: L.ByteString -> [Key]
pKeys bs | L.null bs = []
pKeys bs = key (L.take 4 bs) : pKeys (L.drop 4 bs)

newtype Signature = S (Bool, Int, Int)
  deriving (Eq, Show)


pSignatures :: L.ByteString -> [[Signature]]
pSignatures = runGet getSignatures
  where
    getSignatures :: Get [[Signature]]
    getSignatures = do
        empty <- isEmpty
        if empty
        then return []
        else (:) <$> getSigSet <*> getSignatures
    -- Parse one set of signatures
    getSigSet :: Get [Signature]
    getSigSet = do
        n <- liftM fromIntegral getWord32be
        replicateM n (liftM pSig getWord32be)

pSig :: Word32 -> Signature
pSig b = S (primary, certCheckLevel, sigKey)
  where
    -- The signature type is in the the most significant 4 bits
    sigType = shiftR b 28
    -- Test the 2nd most significant bit to see if the primary id has been signed
    primary = testBit sigType 2
    -- convert the 2 least significant bits to integer
    certCheckLevel = fromIntegral (sigType .&. 0x3)
    -- The rest is the key index
    sigKey = fromIntegral (b .&. 0x0fffffff)
