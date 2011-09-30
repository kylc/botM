{-# LANGUAGE TypeSynonymInstances #-}

module Network.Minecraft.Protocol.MinecraftBinary
    ( MinecraftBinary(..)
    , Data.Binary.Get.getLazyByteString
    )
where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Binary.IEEE754
import Data.ByteString (ByteString)
import Data.Int
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

class MinecraftBinary t where
    getM :: Get t
    putM :: t -> Put

instance MinecraftBinary Bool where
    getM = get
    putM = put

instance MinecraftBinary Word8 where
    getM = get
    putM = put

instance MinecraftBinary Word16 where
    getM = get
    putM = put

instance MinecraftBinary Int8 where
    getM = get
    putM = put

instance MinecraftBinary Int16 where
    getM = get
    putM = put

instance MinecraftBinary Int32 where
    getM = get
    putM = put

instance MinecraftBinary Int64 where
    getM = get
    putM = put

instance MinecraftBinary Float where
    getM = getFloat32be
    putM = putFloat32be

instance MinecraftBinary Double where
    getM = getFloat64be
    putM = putFloat64be

instance MinecraftBinary String where
    getM = do
        len <- getM :: Get Word16
        str <- getByteString $ fromIntegral len * 2
        return $ T.unpack $ T.decodeUtf16BE str

    putM str = do
        putWord16be $ fromIntegral $ length str
        putByteString $ T.encodeUtf16BE $ T.pack str
