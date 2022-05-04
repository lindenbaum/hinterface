{-# LANGUAGE PackageImports #-}
{-# LANGUAGE Strict #-}

module Foreign.Erlang.Digest
  ( genChallenge,
    genDigest,
  )
where

import "cryptonite" Crypto.Hash
  ( MD5 (MD5),
    hashFinalize,
    hashInitWith,
    hashUpdate,
  )
import Data.ByteArray (convert)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as CS
import Data.Word
import System.Random (randomIO)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text

genChallenge :: IO Word32
genChallenge = randomIO

genDigest :: Word32 -> Text -> BS.ByteString
genDigest challenge cookie =
  let ctx0 = hashInitWith MD5
      ctx1 = hashUpdate ctx0 (Text.encodeUtf8 cookie)
      ctx2 = hashUpdate ctx1 (CS.pack (show challenge))
      digest = hashFinalize ctx2
   in convert digest
