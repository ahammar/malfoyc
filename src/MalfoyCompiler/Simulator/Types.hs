
module MalfoyCompiler.Simulator.Types where

import Control.Monad.Error
import qualified Data.Map as M

import MalfoyCompiler.Graph
import MalfoyCompiler.Opcodes

type Tag = Int

data Value = Integer Integer | Reference Label | Target Target | TaggedTarget Target Tag
    deriving (Show)

data DataPacket = DataPacket Target Tag Value
    deriving (Show)

type CAM = M.Map (Label, Tag) Value
type Queue = [DataPacket]

type MachineState = (Queue, CAM, Tag)

data SimulationError = InstructionNotFound Label | QueueUnderflow | UnknownError String
    deriving (Show)

instance Error SimulationError where
    strMsg = UnknownError

data InstructionPacket = Eval Opcode Value (Maybe Value) Tag Target Target
    deriving (Show)
    
data Result = Done Value | Continue

fromConst :: Constant -> Value
fromConst (Literal x) = Integer x
fromConst (Pointer x) = Reference x
