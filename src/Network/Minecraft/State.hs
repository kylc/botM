{-# LANGUAGE FlexibleContexts #-}

module Network.Minecraft.State where

import Control.Monad.State
import Data.Map as M

import Network.Minecraft.Types

-- | Apply the function 'f' to the player state, storing the result.
modifyPlayer :: MonadState BotState m => (Player -> Player) -> m ()
modifyPlayer f =
    modify $ \s -> s {
      statePlayer = f (statePlayer s)
    }

-- | Apply the function 'f' to the entities state, storing the result.
modifyEntities :: MonadState BotState m
               => (Map EntityId Entity -> Map EntityId Entity)
               -> m ()
modifyEntities f =
    modify $ \s -> s {
      stateEntities = f (stateEntities s)
    }

-- | An entity with a reasonable default state.
blankEntity :: EntityId -> Entity
blankEntity eid = Entity eid 0 0 0 0 0 0 True

-- | Add an entity to the state..
addEntity :: MonadState BotState m => Entity -> m ()
addEntity e = modifyEntities $ M.insert (entityId e) e

-- | Add a new entity to the state, unless an the provided 'EntityId' is already
-- taken.
addBlankEntity :: MonadState BotState m => EntityId -> m ()
addBlankEntity eid = modifyEntities $
    M.insertWith' (flip const) eid (blankEntity eid)

-- | Remove an entity from the state.
destroyEntity :: MonadState BotState m => EntityId -> m ()
destroyEntity eid = modifyEntities $ M.delete eid
