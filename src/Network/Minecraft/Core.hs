{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Minecraft.Core
    ( runBot
    , send
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

import Network.Minecraft.Behavior
import Network.Minecraft.IO
import Network.Minecraft.Protocol.Packets
import Network.Minecraft.Types

runBotM :: BotConfig -> BotState -> Bot a -> IO a
runBotM c s (Bot a) =
    flip evalStateT s $
        runReaderT a c

runBot :: BotConfig -> IO ()
runBot co = withSocketsDo $ do
    handle <- connectTo (configHost co) (PortNumber $ fromIntegral $ configPort co)
    hSetBuffering handle NoBuffering

    let st = BotState  { stateHandle = handle
                       , stateWorld = World 0 M.empty
                       , statePlayer = Player 0 0 0 0 0 0 0 False
                       , stateEntities = M.empty
                       }

    messages <- acceptMessages handle

    runBotM co st $ do
      send $ Handshake (configName co)
      send $ LoginRequest 14 (configName co) 0 0

      -- KeepAlive sender
      _ <- liftIO $ forkIO $ forever $ do
        threadDelay $ 1000000 * 10 -- 10 seconds
        sendIO handle KeepAlive

      forM_ messages $ \m ->
        mapM (\b -> b m) (defaultBehaviors ++ configBehaviors co)

    hClose handle

