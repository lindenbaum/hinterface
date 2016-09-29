{-# LANGUAGE PackageImports #-}

module Language.Erlang.Digest
    ( genChallenge
    , genDigest
    ) where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as CS
import           Data.Word
import           Crypto.Hash           ( MD5(MD5), hashFinalize, hashInitWith, hashUpdate )
import           System.Random         ( randomIO )
import           Data.ByteArray        ( convert )

import           Data.IOx

genChallenge :: IOx Word32
genChallenge = toIOx $ do
    randomIO

genDigest :: Word32 -> BS.ByteString -> BS.ByteString
genDigest challenge cookie =
    let ctx0 = hashInitWith MD5
        ctx1 = hashUpdate ctx0 cookie
        ctx2 = hashUpdate ctx1 (CS.pack (show challenge))
        digest = hashFinalize ctx2
    in
        convert digest
