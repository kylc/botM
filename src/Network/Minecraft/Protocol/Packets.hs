{-# LANGUAGE FlexibleInstances #-}

module Network.Minecraft.Protocol.Packets
    ( Message(..)
    , MinecraftBinary(..)
    )
where

import qualified Codec.Compression.Zlib as Zlib
import Control.Applicative
import Control.Monad
import Data.Binary
import Data.Bits
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L

import Network.Minecraft.Protocol.MinecraftBinary
import Network.Minecraft.Types

import Data.Binary.Get (runGet)

fullChunkLength :: Integral a => a
fullChunkLength = 16 * 16 * 128

instance MinecraftBinary Chunk where
    getM = do
        x <- getM :: Get Int32
        y <- getM :: Get Int16
        z <- getM :: Get Int32

        sizeX <- getSize
        sizeY <- getSize
        sizeZ <- getSize

        -- This should equal (sizeX * sizeY * sizeZ) * 2.5 when uncompressed
        compressedSize <- getM :: Get Int32
        decompressed <- Zlib.decompress <$> getLazyByteString (fromIntegral compressedSize)

        let len = sizeX * sizeY * sizeZ

        if len == fullChunkLength
          then return $ runGet (getFullMapChunk len) decompressed
          else return $ runGet (getPartialMapChunk len) decompressed

      where
        -- Make sure to cast these to avoid overflows on the 'Int8' type when
        -- multiplying
        getSize = (+1) <$> (fromIntegral <$> (getM :: Get Word8) :: Get Int)

    putM = undefined

-- TODO: Implement
getFullMapChunk :: Int -> Get Chunk
getFullMapChunk = getPartialMapChunk

getPartialMapChunk :: Int -> Get Chunk
getPartialMapChunk len = do
    blockTypeArray  <- replicateM len (getM :: Get Word8)
    Chunk blockTypeArray <$> getNibbleArray <*> getNibbleArray <*> getNibbleArray
  where
    getNibbleArray = concatMap nibbles <$> replicateM (len `div` 2) (getM :: Get Word8)
    nibbles byte = [top, bottom]
      where top    = (byte .&. 0xf0) `shiftR` 4
            bottom = byte .&. 0x0f

-- TODO: I don't like having this orphan instance here, but I can't put the data
-- declaration here without a circular dependency...
instance MinecraftBinary Message where
    getM = do
        packetId <- getM :: Get Word8
        case packetId of
             0x00 -> return KeepAlive
             0x01 -> LoginRequest              <$> getM <*> getM <*> getM <*> getM
             0x02 -> Handshake                 <$> getM
             0x03 -> ChatMessage               <$> getM
             0x04 -> TimeUpdate                <$> getM
             0x05 -> EntityEquipment           <$> getM <*> getM <*> getM <*> getM
             0x06 -> SpawnPosition             <$> getM <*> getM <*> getM
             0x08 -> UpdateHealth              <$> getM
             0x09 -> Respawn                   <$> getM
             0x0B -> PlayerPosition            <$> getM <*> getM <*> getM <*> getM <*> getM
             0x0C -> PlayerLook                <$> getM <*> getM <*> getM
             0x0D -> PlayerPositionAndLook     <$> getM <*> getM <*> getM <*> getM <*> getM <*> getM <*> getM
             0x0E -> PlayerDigging             <$> getM <*> getM <*> getM <*> getM <*> getM
             0x0F -> PlayerBlockPlacement      <$> getM <*> getM <*> getM <*> getM <*> getM <*> getM <*> getM
             0x10 -> HoldingChange             <$> getM
             0x11 -> UseBed                    <$> getM <*> getM <*> getM <*> getM <*> getM
             0x12 -> Animation                 <$> getM <*> getM
             0x14 -> NamedEntitySpawn          <$> getM <*> getM <*> getM <*> getM <*> getM <*> getM <*> getM <*> getM
             0x15 -> PickupSpawn               <$> getM <*> getM <*> getM <*> getM <*> getM <*> getM <*> getM <*> getM <*> getM <*> getM
             0x16 -> CollectItem               <$> getM <*> getM
             0x17 -> AddObjectOrVehicle        <$> getM <*> getM <*> getM <*> getM <*> getM <*> getM <*> getM <*> getM <*> getM
             0x18 -> MobSpawn                  <$> getM <*> getM <*> getM <*> getM <*> getM <*> getM <*> getM <*> getM
             0x19 -> EntityPainting            <$> getM <*> getM <*> getM <*> getM <*> getM <*> getM
             0x1B -> StanceUpdate              <$> getM <*> getM <*> getM <*> getM <*> getM <*> getM
             0x1C -> EntityVelocity            <$> getM <*> getM <*> getM <*> getM
             0x1D -> DestroyEntity             <$> getM
             0x1E -> EntityUnchanged           <$> getM
             0x1F -> EntityRelativeMove        <$> getM <*> getM <*> getM <*> getM
             0x20 -> EntityLook                <$> getM <*> getM <*> getM
             0x21 -> EntityLookAndRelativeMove <$> getM <*> getM <*> getM <*> getM <*> getM <*> getM
             0x22 -> EntityTeleport            <$> getM <*> getM <*> getM <*> getM <*> getM <*> getM
             0x26 -> EntityStatus              <$> getM <*> getM
             0x27 -> AttachEntity              <$> getM <*> getM
             0x28 -> EntityMetadata            <$> getM <*> getM
             0x32 -> PreChunk                  <$> getM <*> getM <*> getM
             0x33 -> MapChunk                  <$> getM
             0x34 -> MultiBlockChange          <$> getM <*> getM <*> getM
             0x35 -> BlockChange               <$> getM <*> getM <*> getM <*> getM <*> getM
             0x36 -> BlockAction               <$> getM <*> getM <*> getM <*> getM <*> getM
             0x3C -> Explosion                 <$> getM <*> getM <*> getM <*> getM <*> ((getM :: Get Int32) >>= getN)
             0x3D -> SoundEffect               <$> getM <*> getM <*> getM <*> getM <*> getM
             0x46 -> NewInvalidState           <$> getM
             0x47 -> Thunderbolt               <$> getM <*> getM <*> getM <*> getM <*> getM
             0x64 -> OpenWindow                <$> getM <*> getM <*> getM <*> getM -- TODO: This is a String8, not a String16!
             0x65 -> CloseWindow               <$> getM
             0x67 -> SetSlot                   <$> getM <*> getM <*> getM
             0x68 -> WindowItems               <$> getM <*> ((getM :: Get Int16) >>= getN)
             0x69 -> UpdateProgressBar         <$> getM <*> getM <*> getM
             0x6A -> Transaction               <$> getM <*> getM <*> getM
             0x82 -> UpdateSign                <$> getM <*> getM <*> getM <*> replicateM 4 getM
             0x83 -> ItemData                  <$> getM <*> getM <*> ((getM :: Get Word8) >>= getN)
             0xc8 -> IncrementStatistic        <$> getM <*> getM
             0xFF -> Disconnect                <$> getM
             _    -> fail $ "Unexpected packet ID: " ++ show packetId

    putM KeepAlive                              = putWord8 0x00
    putM (LoginRequest v u ms d)                = putWord8 0x01 *> putM v *> putM u*> putM ms *> putM d
    putM (Handshake h)                          = putWord8 0x02 *> putM h
    putM (ChatMessage m)                        = putWord8 0x03 *> putM m
    putM (TimeUpdate t)                         = putWord8 0x04 *> putM t
    putM (EntityEquipment eid s iid u)          = putWord8 0x05 *> putM eid *> putM s *> putM iid *> putM u
    putM (PreChunk x z m)                       = putWord8 0x32 *> putM x *> putM z *> putM m
    putM (PlayerLook y p g)                     = putWord8 0x0C *> putM y *> putM p *> putM g
    putM (PlayerPositionAndLook x s y z yw p g) = putWord8 0x0D *> putM x *> putM y *> putM (73 :: Double) *> putM z *> putM yw *> putM p *> putM g
    putM p = error $ "Undefined packet 'put' for: " ++ show p

getN :: (Integral a, MinecraftBinary b) => a -> Get [b]
getN = flip replicateM getM . fromIntegral

instance MinecraftBinary [Block] where
    getM = do
        arrayLen <- fromIntegral <$> (getM :: Get Int16)
        coordArray    <- replicateM arrayLen getM :: Get [Int16]
        typeArray     <- replicateM arrayLen getM :: Get [Int8]
        metadataArray <- replicateM arrayLen getM :: Get [Int8]

        forM (zip3 coordArray typeArray metadataArray) $ \(c, t, m) -> do
          let (x, y, z) = parseCoords c
          return $ Block x y z t m
      where
        -- TODO: Return Int8's
        parseCoords :: Int16 -> (Int16, Int16, Int16)
        parseCoords s =
            let compX = (s .&. 0xf000) `shiftL` 12
                compY = (s .&. 0x0f00) `shiftL` 8
                compZ = (s .&. 0x00ff)
            in  (compX, compY, compZ)

    putM = undefined

instance MinecraftBinary Metadata where
    getM = takeWhileNot 127
      where
        takeWhileNot n = do
          x <- getM :: Get Word8
          if x == n
            then return $ Metadata "HELLO WORLD"  -- TODO
            else takeWhileNot n

    putM = undefined

instance MinecraftBinary InventoryItem where
    getM = do
      itemId <- getM
      if itemId /= 65535
        then InventoryItem itemId <$> getM <*> getM
        else return EmptySlot

    putM = undefined
