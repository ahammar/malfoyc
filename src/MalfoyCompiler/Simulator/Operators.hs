
module MalfoyCompiler.Simulator.Operators where

type RegularUnaryOperator  = Value -> Value
type RegularBinaryOperator = Value -> Value -> Value

type SpecialOperator = InstructionPacket -> Sim [DataPacket]

type Operator = InstructionPacket -> Sim [DataPacket]



