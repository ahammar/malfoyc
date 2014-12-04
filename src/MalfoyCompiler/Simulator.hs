
module Main where

import Control.Monad
import Control.Monad.Trans
import System.Environment
import System.Exit
import System.IO

import MalfoyCompiler.Compiler
import MalfoyCompiler.Graph
import MalfoyCompiler.Opcodes (Opcode, Arity(..), arity)
import qualified MalfoyCompiler.Opcodes as Op

import MalfoyCompiler.Simulator.Monad
import MalfoyCompiler.Simulator.Types

-- | main function for the simulator
main = do
    input <- getArgs >>= readFile . head
    graph <- case compile input of
        Left error -> do
            hPutStrLn stderr (show error)
            exitWith (ExitFailure 1)
        Right graph -> return graph

    args  <- getArgs >>= (return . map (Integer . read) . tail)

    result <- simulate graph args
    case result of
        (Left err)  -> print err
        (Right val) -> print val

-- | Simulates the execution of a program given some arguments.
simulate :: Graph -> [Value] -> IO (Either SimulationError Value)
simulate graph args = runSim (loadArgs args >> run) graph

-- | Runs the simulation until it is finished.
run :: Sim Value
run = do 
    result <- step
    case result of
        Done val -> return val
        Continue -> run

-- | Runs a single simulation step.
step :: Sim Result
step = fetch >>= match >>= maybe continue evaluate

-- | Attempts to prepare an instruction packet from the given data packet.
-- Returns Nothing and stores the packet in CAM if if the packet could not
-- be matched at this time.
match :: DataPacket -> Sim (Maybe InstructionPacket)
match (DataPacket target tag value) = do
    let Just (side, label) = target
    (Instruction opcode t0 t1 const) <- lookupInstruction label

    case arity opcode of
        Unary  -> return . Just $ Eval opcode value Nothing tag t0 t1
        Binary -> do
            other <- case const of
                Just const -> return . Just $ fromConst const
                Nothing    -> lookupCAM label tag value

            case other of
                Just other -> do
                    let (val0, val1) = combine side value other
                    return . Just $ Eval opcode val0 (Just val1) tag t0 t1
                Nothing    -> return Nothing

  where
    combine :: Side -> Value -> Value -> (Value, Value)
    combine side val0 val1 =
        case side of
            Lhs -> (val0, val1)
            Rhs -> (val1, val0)

-- Evaluates an instruction packet.
evaluate :: InstructionPacket -> Sim Result

evaluate (Eval Op.Seq val0 (Just val1) tag t0 t1) = do
    enqueue (DataPacket t0 tag val0)
    enqueue (DataPacket t1 tag val1)
    continue

evaluate (Eval Op.Call (Reference x) Nothing tag t0 t1) = do
    enqueue (DataPacket (lhs x) tag (Target t0))
    enqueue (DataPacket (rhs x) tag (Target t1))
    continue

evaluate (Eval Op.Context (Target x) (Just (Target y)) tag t0 t1) = do
    tag' <- newTag
    enqueue (DataPacket t0 tag' (TaggedTarget x tag))
    enqueue (DataPacket y  tag  (TaggedTarget t1 tag'))
    continue

evaluate (Eval Op.Arg (TaggedTarget x tag') (Just y) tag t0 Nothing) = do
    enqueue (DataPacket x tag' y)
    enqueue (DataPacket x tag' (TaggedTarget t0 tag))
    continue

evaluate (Eval Op.Param (TaggedTarget x tag) (Just y) tag' t0 t1) = do
    enqueue (DataPacket t0 tag' y)
    enqueue (DataPacket x tag (TaggedTarget t1 tag'))
    continue

evaluate (Eval Op.Ret (TaggedTarget x tag) (Just y) tag' Nothing Nothing) = do
    freeTag tag'
    enqueue (DataPacket x tag y)
    continue

evaluate (Eval Op.Branch (Integer x) (Just y) tag t0 t1) = do
    if x /= 0
        then enqueue (DataPacket t0 tag y)
        else enqueue (DataPacket t1 tag y)
    continue

evaluate (Eval Op.Exit x Nothing tag Nothing Nothing) = do
    finish x 

evaluate (Eval opcode value Nothing tag t0 t1) = do
    result <- case opcode of
        Op.Dup -> return value
        otherwise -> fail ("Unknown unary opcode: " ++ show opcode)
       
    enqueue (DataPacket t0 tag result)
    enqueue (DataPacket t1 tag result)
    continue 

evaluate (Eval opcode (Integer val0) (Just (Integer val1)) tag t0 t1) = do
    result <- case opcode of
        Op.Equals    -> return $ bool (val0 == val1)
        Op.LessThan  -> return $ bool (val0 < val1)
        Op.Add       -> return (val0 + val1)
        Op.Sub       -> return (val0 - val1)
        Op.Mul       -> return (val0 * val1)
        otherwise -> fail ("Unknown binary opcode: " ++ show opcode)

    enqueue (DataPacket t0 tag $ Integer result)
    enqueue (DataPacket t1 tag $ Integer result)
    continue

    where bool :: Bool -> Integer
          bool True  = -1
          bool False = 0

evaluate packet = do
    liftIO $ putStrLn ("Not implemented: " ++ show packet)
    continue

-- | Loads the specified arguments into the data packet queue with the
-- correct target destinations.
loadArgs :: [Value] -> Sim ()
loadArgs args = mapM_ enqueue [DataPacket (rhs a3) 0 arg | arg <- args]
    where a3 = (succ (succ (succ minBound)))

-- | Indicates that the simulation should continue.
continue :: Sim Result
continue = return Continue

-- | Indicates that the simulation is finished.
finish :: Value -> Sim Result
finish = return . Done


