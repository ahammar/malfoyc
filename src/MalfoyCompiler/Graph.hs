
 {-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MalfoyCompiler.Graph (Label, Side(..), Target(..), Instruction(..), Constant(..), Graph, lhs, rhs, nowhere) where

import Data.Char (toLower)
import qualified Data.Map as M
import Data.Word

import MalfoyCompiler.Opcodes (Opcode)

-- | Represents the graph of a program.
type Graph = M.Map Label Instruction

data Instruction = Instruction Opcode Target Target (Maybe Constant)
    deriving (Show)

-- | Represents which input of an instruction a value should be sent to.
data Side = Lhs | Rhs
    deriving (Show)

-- | Represents the place where an output of an instruction will be sent.
type Target = Maybe (Side, Label)

lhs :: Label -> Target
lhs lbl = Just (Lhs, lbl)

rhs :: Label -> Target
rhs lbl = Just (Rhs, lbl)

nowhere :: Target
nowhere = Nothing

-- | Represents a constant value in the program graph. A constant value
-- replaces one of the inputs to an instruction. 
data Constant = Literal Integer | Pointer Label
    deriving (Show)

-- | Labels are used for identifying instruction nodes in the graph.
newtype Label = Label Word32
    deriving (Bounded, Enum, Ord, Eq)

-- FIXME: abuse of the Show typeclass.
instance Show Label where
    show (Label x) = "_a" ++ show x

