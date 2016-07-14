module Language.Erlang.NodeDataSpec ( spec
                                    )
       where

import Test.Hspec
--import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.Word
import Data.List (nub, sort)
import Data.Binary (encode, decode)
import Data.Binary.Put (runPut)
import Data.Binary.Get (runGet)

import Language.Erlang.NodeData

spec :: Spec
spec = do
  describe "NodeType" $ do
    it "encodes/decodes properly" $ property $ do
      a <- arbitraryBoundedEnum :: Gen NodeType
      return $ (decode . encode) a `shouldBe` a
    it "NormalNode encodes to 77" $
      encode NormalNode `shouldBe` encode (77 :: Word8)
    it "HiddenNode encodes to 72" $
      encode HiddenNode `shouldBe` encode (72 :: Word8)


  describe "NodeProtocol" $ do
    it "encodes/decodes properly" $ property $ do
      a <- arbitraryBoundedEnum :: Gen NodeProtocol
      return $ (decode . encode) a `shouldBe` a
    it "TcpIpV4 encodes to 0" $
      encode TcpIpV4 `shouldBe` encode (0 :: Word8)

  describe "DistributionVersion" $ do
    it "encodes/decodes properly" $ property $ do
      a <- arbitraryBoundedEnum :: Gen DistributionVersion
      return $ (decode . encode) a `shouldBe` a
    it "R4 encodes to 1" $
      encode R4 `shouldBe` encode (1 :: Word16)
    it "R5C encodes to 3" $
      encode R5C `shouldBe` encode (3 :: Word16)
    it "R6 encodes to 4" $
      encode R6 `shouldBe` encode (4 :: Word16)
    it "R6B encodes to 5" $
      encode R6B `shouldBe` encode (5 :: Word16)

  describe "DistributionFlags" $ do
    it "encodes/decodes properly" $ property $ do
      a <- (nub . sort) <$> listOf arbitraryBoundedEnum :: Gen DistributionFlags
      return $ ((runGet getDistributionFlags) . (runPut . putDistributionFlags)) a `shouldBe` a
    it "[] encodes to 0x00000" $
      (runPut . putDistributionFlags) [] `shouldBe` encode (0x00000 :: Word32)
    it "[PUBLISHED] encodes to 0x00001" $
      (runPut . putDistributionFlags) [PUBLISHED] `shouldBe` encode (0x00001 :: Word32)
    it "[ATOM_CACHE] encodes to 0x00002" $
      (runPut . putDistributionFlags) [ATOM_CACHE] `shouldBe` encode (0x00002 :: Word32)
    it "[EXTENDED_REFERENCES] encodes to 0x00004" $
      (runPut . putDistributionFlags) [EXTENDED_REFERENCES] `shouldBe` encode (0x00004 :: Word32)
    it "[DIST_MONITOR] encodes to 0x00008" $
      (runPut . putDistributionFlags) [DIST_MONITOR] `shouldBe` encode (0x00008 :: Word32)
    it "[FUN_TAGS] encodes to 0x00010" $
      (runPut . putDistributionFlags) [FUN_TAGS] `shouldBe` encode (0x00010 :: Word32)
    it "[DIST_MONITOR_NAME] encodes to 0x00020" $
      (runPut . putDistributionFlags) [DIST_MONITOR_NAME] `shouldBe` encode (0x00020 :: Word32)
    it "[HIDDEN_ATOM_CACHE] encodes to 0x00040" $
      (runPut . putDistributionFlags) [HIDDEN_ATOM_CACHE] `shouldBe` encode (0x00040 :: Word32)
    it "[NEW_FUN_TAGS] encodes to 0x00080" $
      (runPut . putDistributionFlags) [NEW_FUN_TAGS] `shouldBe` encode (0x00080 :: Word32)
    it "[EXTENDED_PIDS_PORTS] encodes to 0x00100" $
      (runPut . putDistributionFlags) [EXTENDED_PIDS_PORTS] `shouldBe` encode (0x00100 :: Word32)
    it "[EXPORT_PTR_TAG] encodes to 0x00200" $
      (runPut . putDistributionFlags) [EXPORT_PTR_TAG] `shouldBe` encode (0x00200 :: Word32)
    it "[BIT_BINARIES] encodes to 0x00400" $
      (runPut . putDistributionFlags) [BIT_BINARIES] `shouldBe` encode (0x00400 :: Word32)
    it "[NEW_FLOATS] encodes to 0x00800" $
      (runPut . putDistributionFlags) [NEW_FLOATS] `shouldBe` encode (0x00800 :: Word32)
    it "[UNICODE_IO] encodes to 0x01000" $
      (runPut . putDistributionFlags) [UNICODE_IO] `shouldBe` encode (0x01000 :: Word32)
    it "[DIST_HDR_ATOM_CACHE] encodes to 0x02000" $
      (runPut . putDistributionFlags) [DIST_HDR_ATOM_CACHE] `shouldBe` encode (0x02000 :: Word32)
    it "[SMALL_ATOM_TAGS] encodes to 0x04000" $
      (runPut . putDistributionFlags) [SMALL_ATOM_TAGS] `shouldBe` encode (0x04000 :: Word32)
    it "[UTF8_ATOMS] encodes to 0x10000" $
      (runPut . putDistributionFlags) [UTF8_ATOMS] `shouldBe` encode (0x10000 :: Word32)
