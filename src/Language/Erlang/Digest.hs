{-# LANGUAGE PackageImports #-}

module Language.Erlang.Digest ( genChallenge
                              , genDigest
                              )
       where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as CS
import Data.Word
import "cryptonite" Crypto.Hash (hashInitWith, hashUpdate, hashFinalize, MD5(MD5))
import System.Random (randomIO)
import Data.ByteArray (convert)

import Util.IOx

genChallenge :: IOx Word32
genChallenge = toIOx $ do
  randomIO

genDigest :: Word32 -> BS.ByteString -> BS.ByteString
genDigest challenge cookie =
  let
    ctx0 = hashInitWith MD5
    ctx1 = hashUpdate ctx0 cookie
    ctx2 = hashUpdate ctx1 (CS.pack (show challenge))
    digest = hashFinalize ctx2
  in
    convert digest
