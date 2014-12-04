
module MalfoyCompiler.Opcodes where

import Data.Char (toLower)

data Opcode = Add
            | Sub
            | Mul
            | Div
            | And
            | Not
            | Equals
            | LessThan
            | Dup
            | Call
            | Ret
            | Seq
            | Branch
            | Context
            | Param
            | Arg
            | Exit
    deriving (Show, Eq)

data Arity = Unary | Binary

-- | Returns the short literal name of an operation.
mnemonic :: Opcode -> String
mnemonic = map toLower . show

-- | Regular instructions apply some function to their inputs
-- and returns the same value on both their outputs.
regular :: Opcode -> Bool
regular = not . special

-- | Special instructions do not follow the regular instruction behavior.
special :: Opcode -> Bool
special = flip elem [Call, Ret, Seq, Branch, Context, Param, Arg, Exit]

-- | Unary instructions only require one input.
unary :: Opcode -> Bool
unary = flip elem [Not, Dup, Call, Exit]

arity :: Opcode -> Arity
arity opcode =
    if elem opcode [Not, Dup, Call, Exit]
        then Unary
        else Binary

