module Main where

import Control.Concurrent
import Control.Monad.State
import Data.List
import qualified Data.Map as M

import Network.Minecraft.Actions
import Network.Minecraft.Core
import Network.Minecraft.State
import Network.Minecraft.Types

type Behavior = Message -> Bot ()

debugR :: Behavior
-- debugR x@(NamedEntitySpawn eid name _ _ _ _ _ _) = liftIO $ print x
-- debugR x@(EntityUnchanged eid) = liftIO $ print x
-- debugR x@(MultiBlockChange _ _ blocks) = liftIO $ print (map blockType blocks)
debugR x@(MapChunk _) = liftIO $ print x
debugR x@(PlayerPositionAndLook _ _ _ _ _ _ _) = liftIO $ print x
debugR x = do
    liftIO $ putStrLn $ "<<< " ++ show x
    move 1 0 1

-- A simple behavior that chats "Hello, world!" when the ot finishes connecting.
joinR :: Behavior
joinR LoginRequest {} = chat "Hello, world!"
joinR _ = return ()

main :: IO ()
main = runBot config
  where
    config = BotConfig { configHost = "127.0.0.1"
                       , configPort = 25565
                       , configName = "JustaBot"
                       , configBehaviors = behaviors
                       }
    behaviors = [debugR, joinR]
