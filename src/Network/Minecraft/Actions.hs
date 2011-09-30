module Network.Minecraft.Actions
    ( chat
    , look
    , move
    ) where

import Control.Monad.State

import Network.Minecraft.Core (send)
import Network.Minecraft.State
import Network.Minecraft.Types

chat :: String -> Bot ()
chat = send . ChatMessage

look :: Float -> Float -> Bot ()
look y p = modifyPlayer $ \s -> s { playerYaw = y, playerPitch = p }

move :: Int -> Int -> Int -> Bot ()
move dx dy dz = do
    modifyPlayer $ \s -> s { playerX = playerX s + fromIntegral dx
                           , playerY = playerY s + fromIntegral dy
                           , playerZ = playerZ s + fromIntegral dz
                           }
    player <- gets statePlayer
    send $ PlayerPosition (playerX player) (playerY player) (playerStance player) (playerZ player) (playerOnGround player)

