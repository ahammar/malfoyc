{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MalfoyCompiler.Simulator.Monad (
    Sim, runSim,
    fetch, enqueue, lookupInstruction, lookupCAM, newTag, freeTag
) where

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M

import MalfoyCompiler.Graph
import MalfoyCompiler.Opcodes
import MalfoyCompiler.Simulator.Types

newtype Sim a = Sim (ReaderT Graph (ErrorT SimulationError (StateT MachineState IO)) a)
    deriving (Functor, Monad, MonadError SimulationError, MonadIO)

-- | Fetch a data packet from the queue
fetch :: Sim DataPacket
fetch = Sim $ do
    (queue, cam, tag) <- get
    case queue of
        (p:ps) -> do put (ps, cam, tag); return p
        []     -> throwError QueueUnderflow

-- | Put a data packet onto the queue. Packets with no target are ignored.
enqueue :: DataPacket -> Sim ()
enqueue packet@(DataPacket target _ _) = Sim $ do
    case target of
        Just (side, label) -> do
            (queue, cam, tag) <- get
            put (queue ++ [packet], cam, tag)
        Nothing -> return ()

-- | Retrieves an instruction node from the program graph.
lookupInstruction :: Label -> Sim Instruction
lookupInstruction label = do
    Sim ask >>= maybe (throwError $ InstructionNotFound label) return . M.lookup label

-- | Returns a matching value from the CAM. If none is found, the specified
-- value is inserted so that it may be matched with another later.
lookupCAM :: Label -> Tag -> Value -> Sim (Maybe Value)
lookupCAM label tag value = Sim get >>= \(queue, cam, nextTag) -> do
    case M.lookup (label, tag) cam of
        Just value -> do
            Sim $ put (queue, M.delete (label, tag) cam, nextTag)
            return $ Just value
        Nothing -> do
            Sim $ put (queue, M.insert (label, tag) value cam, nextTag)
            return Nothing

-- | Runs the specified simulation on the specified graph.
runSim :: Sim a -> Graph -> IO (Either SimulationError a)
runSim (Sim s) graph = evalStateT (runErrorT (runReaderT s graph)) initial
    where initial = ([], M.empty, 0)

-- | Returns a new unique tag.
-- TODO: Reuse tags?
newTag :: Sim Tag
newTag = Sim get >>= \(queue, cam, nextTag) -> do
    Sim $ put (queue, cam, nextTag+1)
    return (nextTag)

-- | Marks a tag available for reuse.
-- TODO: Reuse tags?
freeTag :: Tag -> Sim ()
freeTag tag = return () 
