{-# LANGUAGE BangPatterns #-}

module Network.Minecraft.IO 
    ( acceptMessages
    , send
    , sendIO
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import Network
import System.IO

import Network.Minecraft.Types
import Network.Minecraft.Protocol.MinecraftBinary
import Network.Minecraft.Protocol.Packets

-- | Produces a lazy list of messages read from 'handle'.
acceptMessages :: Handle -> IO [Message]
acceptMessages handle = do
    input <- liftIO $ L.hGetContents handle
    return $ go input 0
  where
    go :: ByteString -> Int64 -> [Message]
    go input offset = do
      let (!message, input', offset') = B.runGetState getM input offset
      message : go input' offset'

-- | Send a message to the server.
send :: Message -> Bot ()
send m = do
  h <- gets stateHandle
  liftIO $ sendIO h m

-- | Send a message outside of a 'Bot' context.  Internal use only.
sendIO :: Handle -> Message -> IO ()
sendIO h = L.hPut h . B.runPut . putM
