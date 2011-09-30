module Network.Minecraft.Behavior 
  ( defaultBehaviors
  ) where

import Control.Monad.State
import Data.Map as M

import Network.Minecraft.IO
import Network.Minecraft.State
import Network.Minecraft.Types

-- Convenience for defining behaviors.
type Behavior = Message -> Bot ()

-- Updates 'World' state when the server notifies us of a time change.
timeKeepBehavior :: Behavior
timeKeepBehavior (TimeUpdate time) =
    modify $ \s -> s {
      stateWorld = (stateWorld s) {
        worldTime = time
      }
    }
timeKeepBehavior _ = return ()

-- Handles all entity updating.  This includes entity spawning, entity
-- destroying, entity looks, and entity moves.  This handler updates the 'World'
-- state with all the information it receives.
entityUpdateBehavior :: Behavior
entityUpdateBehavior (NamedEntitySpawn eid name x y z yaw pitch item) =
    addEntity $ Entity eid x y z 0 yaw pitch True
entityUpdateBehavior (EntityUnchanged eid) = addBlankEntity eid
entityUpdateBehavior (DestroyEntity eid) = destroyEntity eid
entityUpdateBehavior (EntityRelativeMove eid dx dy dz) = updateEntityPosition eid dx dy dz
entityUpdateBehavior (EntityLook eid yaw pitch) = updateEntityLook eid yaw pitch
entityUpdateBehavior (EntityLookAndRelativeMove eid dx dy dz yaw pitch) =
    updateEntityPosition eid dx dy dz >> updateEntityLook eid yaw pitch
entityUpdateBehavior _ = return ()

updateEntityPosition :: EntityId -> Int8 -> Int8 -> Int8 -> Bot ()
updateEntityPosition eid dx dy dz = modifyEntities $ M.adjust updatePosition eid
  where updatePosition e = e { entityX = fromIntegral dx + entityX e
                             , entityY = fromIntegral dy + entityY e
                             , entityZ = fromIntegral dz + entityZ e
                             }

updateEntityLook :: EntityId -> Int8 -> Int8 -> Bot ()
updateEntityLook eid yaw pitch = modifyEntities $ M.adjust updateLook eid
  where updateLook e = e { entityYaw = yaw
                         , entityPitch = pitch
                         }

updatePlayerBehavior :: Behavior
updatePlayerBehavior p@(PlayerPositionAndLook x stance y z yaw pitch ground) = do
    modifyPlayer $ \p -> p { playerX = x
                           , playerStance = stance
                           , playerY = y
                           , playerZ = z
                           , playerYaw = yaw
                           , playerPitch = pitch
                           , playerOnGround = ground
                           }
    send p 
updatePlayerBehavior _ = return ()

defaultBehaviors :: [Behavior]
defaultBehaviors = [entityUpdateBehavior, timeKeepBehavior, updatePlayerBehavior]
