module Util.Binary
    ( runGetA
    , runPutA
    , putFloatbe
    , putFloatle
    , putFloathost
    , putDoublebe
    , putDoublele
    , putDoublehost
    , putLength16beByteString
    , putLength32beByteString
    , putWithLength16be
    , putWithLength32be
    , putChar8
    , getChar8
    , getFloatbe
    , getFloatle
    , getFloathost
    , getDoublebe
    , getDoublele
    , getDoublehost
    , getLength8ByteString
    , getLength16beByteString
    , getLength32beByteString
    , getWithLength16be
    , matchWord8
    , matchChar8
    ) where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Word
import           Data.Char

import           Util.FloatCast

--------------------------------------------------------------------------------
runGetA :: (Monad m) => (Int -> m BS.ByteString) -> (BS.ByteString -> m ()) -> Get a -> m (Either String a) -- FIXME improve String error type
runGetA readA unreadA getA =
    feed (runGetIncremental getA) (readA 2048)
  where
    --feed :: Decoder a -> m BS.ByteString -> m a
    feed (Done unused _pos output) _input = do
        unreadA unused
        return $ Right output
    feed (Fail unused pos msg) _input = do
        unreadA unused
        return $ Left $ "runGetA: pos " ++ show pos ++ ": " ++ show msg
    feed (Partial k) input = do
        chunk <- input
        if BS.null chunk then feed (k Nothing) input else feed (k (Just chunk)) input

runPutA :: (LBS.ByteString -> m ()) -> Put -> m ()
runPutA = (. runPut)

--------------------------------------------------------------------------------
------------------------------------------------------------------------
-- Floats/Doubles
-- | Write a 'Float' in big endian IEEE-754 format.
putFloatbe :: Float -> Put
putFloatbe = putWord32be . floatToWord

{-# INLINE putFloatbe #-}

-- | Write a 'Float' in little endian IEEE-754 format.
putFloatle :: Float -> Put
putFloatle = putWord32le . floatToWord

{-# INLINE putFloatle #-}

-- | Write a 'Float' in native in IEEE-754 format and host endian.
putFloathost :: Float -> Put
putFloathost = putWord32host . floatToWord

{-# INLINE putFloathost #-}

-- | Write a 'Double' in big endian IEEE-754 format.
putDoublebe :: Double -> Put
putDoublebe = putWord64be . doubleToWord

{-# INLINE putDoublebe #-}

-- | Write a 'Double' in little endian IEEE-754 format.
putDoublele :: Double -> Put
putDoublele = putWord64le . doubleToWord

{-# INLINE putDoublele #-}

-- | Write a 'Double' in native in IEEE-754 format and host endian.
putDoublehost :: Double -> Put
putDoublehost = putWord64host . doubleToWord

{-# INLINE putDoublehost #-}

--------------------------------------------------------------------------------
putLength16beByteString :: BS.ByteString -> Put
putLength16beByteString bs = do
    putWord16be (fromIntegral (BS.length bs))
    putByteString bs

putLength32beByteString :: BS.ByteString -> Put
putLength32beByteString bs = do
    putWord32be (fromIntegral (BS.length bs))
    putByteString bs

--------------------------------------------------------------------------------
putWithLength16be :: Put -> Put
putWithLength16be putA = do
    let bl = runPut putA
        len = LBS.length bl
    putWord16be (fromIntegral len)
    putLazyByteString bl

putWithLength32be :: Put -> Put
putWithLength32be putA = do
    let bl = runPut putA
        len = LBS.length bl
    putWord32be (fromIntegral len)
    putLazyByteString bl

--------------------------------------------------------------------------------
putChar8 :: Char -> Put
putChar8 c = do
    putWord8 $ fromIntegral $ ord c

getChar8 :: Get Char
getChar8 = (chr . fromIntegral) <$> getWord8

------------------------------------------------------------------------
-- Double/Float reads
-- | Read a 'Float' in big endian IEEE-754 format.
getFloatbe :: Get Float
getFloatbe = wordToFloat <$> getWord32be

{-# INLINE getFloatbe #-}

-- | Read a 'Float' in little endian IEEE-754 format.
getFloatle :: Get Float
getFloatle = wordToFloat <$> getWord32le

{-# INLINE getFloatle #-}

-- | Read a 'Float' in IEEE-754 format and host endian.
getFloathost :: Get Float
getFloathost = wordToFloat <$> getWord32host

{-# INLINE getFloathost #-}

-- | Read a 'Double' in big endian IEEE-754 format.
getDoublebe :: Get Double
getDoublebe = wordToDouble <$> getWord64be

{-# INLINE getDoublebe #-}

-- | Read a 'Double' in little endian IEEE-754 format.
getDoublele :: Get Double
getDoublele = wordToDouble <$> getWord64le

{-# INLINE getDoublele #-}

-- | Read a 'Double' in IEEE-754 format and host endian.
getDoublehost :: Get Double
getDoublehost = wordToDouble <$> getWord64host

{-# INLINE getDoublehost #-}

--------------------------------------------------------------------------------
getLength8ByteString :: Get BS.ByteString
getLength8ByteString = getWord8 >>= getByteString . fromIntegral

getLength16beByteString :: Get BS.ByteString
getLength16beByteString =
    getWord16be >>= getByteString . fromIntegral

getLength32beByteString :: Get BS.ByteString
getLength32beByteString =
    getWord32be >>= getByteString . fromIntegral

--------------------------------------------------------------------------------
getWithLength16be :: Get a -> Get (a, Word16)
getWithLength16be getA = do
    pos0 <- bytesRead
    res <- getA
    pos1 <- bytesRead
    return (res, fromIntegral (pos1 - pos0))

--------------------------------------------------------------------------------
matchWord8 :: Word8 -> Get ()
matchWord8 expected = do
    actual <- getWord8
    if expected == actual then return () else fail $ "expected " ++ show expected ++ ", actual " ++ show actual

matchChar8 :: Char -> Get ()
matchChar8 expected = do
    matchWord8 $ fromIntegral $ ord expected--------------------------------------------------------------------------------
