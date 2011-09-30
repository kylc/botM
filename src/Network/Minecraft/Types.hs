{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Minecraft.Types
    ( Bot(..)
    , BotConfig(..)
    , BotState(..)
    , World(..)
    , EntityId
    , Player(..)
    , Entity(..)
    , InventoryItem(..)
    , Message(..)
    , Chunk(..)
    , Block(..)
    , Metadata(..)

    , module Data.Int
    ) where

import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString (ByteString)
import Data.Int
import Data.Map (Map)
import qualified Data.Map as M
import Data.Ord (comparing)
import Data.Word
import System.IO (Handle)

-- | The Bot monad.  'ReaderT' and 'StateT' transformers over 'IO' encapsulating
-- the bot's configuration and state.
newtype Bot a = Bot (ReaderT BotConfig (StateT BotState IO) a)
    deriving (Monad, Functor, MonadIO, MonadReader BotConfig, MonadState BotState)

-- | The read-only configuration of the bot.
data BotConfig = BotConfig
    { configHost      :: String
    , configPort      :: Int
    , configName      :: String
    , configBehaviors :: [Message -> Bot ()]
    }

-- | The mutable state of the bot.
data BotState = BotState
    { stateHandle   :: Handle
    , stateWorld    :: World
    , statePlayer   :: Player
    , stateEntities :: Map EntityId Entity
    }

data World = World
    { worldTime     :: Int64
    , worldEntities :: Map Int32 Entity
    }

-- | An entity's identification number.
type EntityId = Int32

-- | Our player.
data Player = Player
    { playerId       :: EntityId
    , playerX        :: Double
    , playerY        :: Double
    , playerZ        :: Double
    , playerStance   :: Double
    , playerYaw      :: Float
    , playerPitch    :: Float
    , playerOnGround :: Bool
    }

-- | Any item, player, mob, minecart, or boat in the world.
data Entity = Entity
    { entityId       :: EntityId
    , entityX        :: Int32
    , entityY        :: Int32
    , entityZ        :: Int32
    , entityStance   :: Int32
    , entityYaw      :: Int8
    , entityPitch    :: Int8
    , entityOnGround :: Bool
    } deriving (Show)

instance Eq Entity where
    a == b = entityId a == entityId b

instance Ord Entity where
    a `compare `b = comparing entityId a b

data InventoryItem = InventoryItem
    { invId    :: Int16
    , invCount :: Int8
    , invUses  :: Int16
    }
    | EmptySlot
    deriving (Eq, Show)

data Chunk = Chunk
    { chunkBlockTypeArray  :: [Word8]
    , chunkMetadataArray   :: [Word8]
    , chunkBlockLightArray :: [Word8]
    , chunkSkyLightArray   :: [Word8]
    } deriving (Eq, Show)

data Block = Block
    { blockX        :: Int16
    , blockY        :: Int16
    , blockZ        :: Int16
    , blockType     :: Int8
    , blockMetadata :: Int8
    } deriving (Eq, Show)

-- TODO: Implement this
data Metadata = Metadata String
    deriving (Eq, Show)

-- | A protocol message, sent or received on the wire.
data Message = KeepAlive
             | LoginRequest Int32 String Int64 Int8
             | Handshake String
             | ChatMessage String
             | TimeUpdate Int64 -- Server to Client only
             | EntityEquipment Int32 Int16 Int16 Int16
             | SpawnPosition Int32 Int32 Int32 -- Server to Client only
             | UseEntity Int32 Int32 Bool
             | UpdateHealth Int16 -- Server to Client only
             | Respawn Int8
             | PlayerOnGround Bool
             | PlayerPosition Double Double Double Double Bool
             | PlayerLook Float Float Bool
             | PlayerPositionAndLook Double Double Double Double Float Float Bool
             | PlayerDigging Int8 Int32 Int8 Int32 Int8
             | PlayerBlockPlacement Int32 Int8 Int32 Int8 Int16 Int8 Int16
             | HoldingChange Int16
             | UseBed Int32 Int8 Int32 Int8 Int32 -- Server to Client only
             | Animation Int32 Int8
             | EntityAction Int32 Int8
             | NamedEntitySpawn Int32 String Int32 Int32 Int32 Int8 Int8 Int16 -- Server to Client only
             | PickupSpawn Int32 Int16 Int8 Int16 Int32 Int32 Int32 Int8 Int8 Int8
             | CollectItem Int32 Int32
             | AddObjectOrVehicle Int32 Int8 Int32 Int32 Int32 Int32 Int16 Int16 Int16 -- Server to Client only
             | MobSpawn Int32 Int8 Int32 Int32 Int32 Int8 Int8 Metadata -- Server to Client only
             | EntityPainting Int32 String Int32 Int32 Int32 Int32
             | StanceUpdate Float Float Float Float Bool Bool
             | EntityVelocity Int32 Int16 Int16 Int16
             | DestroyEntity Int32 -- Server to Client only
             | EntityUnchanged Int32 -- Server to Client only
             | EntityRelativeMove Int32 Int8 Int8 Int8 -- Server to Client only
             | EntityLook Int32 Int8 Int8 -- Server to Client only
             | EntityLookAndRelativeMove Int32 Int8 Int8 Int8 Int8 Int8 -- Server to Client only
             | EntityTeleport Int32 Int32 Int32 Int32 Int8 Int8 -- Server to Client only
             | EntityStatus Int32 Int8 -- Server to Client only
             | AttachEntity Int32 Int32 -- Server to Client only
             | EntityMetadata Int32 Metadata
             | PreChunk Int32 Int32 Bool -- Server to Client only
             | MapChunk Chunk -- Server to Client only
             | MultiBlockChange Int32 Int32 [Block]
             | BlockChange Int32 Int8 Int32 Int8 Int8
             | BlockAction Int32 Int16 Int32 Int8 Int8
             | Explosion Int64 Int64 Int64 Float [Word8]
             | SoundEffect Int32 Int32 Int8 Int32 Int32
             | NewInvalidState Int8
             | Thunderbolt Int32 Bool Int32 Int32 Int32
             | OpenWindow Int8 Int8 String Int8
             | CloseWindow Int8
             | WindowClick Int8 Int16 Int8 Int16 Bool Int16 Int8 Int16
             | SetSlot Int8 Int16 InventoryItem
             | WindowItems Int8 [InventoryItem]
             | UpdateProgressBar Int8 Int16 Int16
             | Transaction Int8 Int16 Bool
             | UpdateSign Int32 Int16 Int32 [String]
             | ItemData Int16 Int16  [Int8]
             | IncrementStatistic Int32 Int8
             | Disconnect String
    deriving (Eq, Show)

