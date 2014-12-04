
module MalfoyCompiler.Assembly (toAssembly) where

import qualified Data.Map as M
import Data.Maybe

import MalfoyCompiler.Graph
import MalfoyCompiler.Opcodes (mnemonic)

-- | Convert a program graph to its assembly representation.
toAssembly :: Graph -> String
toAssembly graph = unlines . map showLine $ M.toList graph
    where showLine (label, instr) = toAssemblyLabel label ++ " " ++ toAssemblyInstr instr

toAssemblyInstr :: Instruction -> String
toAssemblyInstr (Instruction opcode t0 t1 const) =
    unwords ([mnemonic opcode, toAssemblyTarget t0, toAssemblyTarget t1] ++ maybeToList (fmap toAssemblyConstant const))

toAssemblyTarget :: Target -> String
toAssemblyTarget target = case target of
    Nothing -> "---"
    Just (Lhs, label) -> toAssemblyLabel label ++ ":l"
    Just (Rhs, label) -> toAssemblyLabel label ++ ":r"

toAssemblyConstant :: Constant -> String
toAssemblyConstant constant = case constant of
    (Literal value) -> '$' : show value
    (Pointer label) -> '&' : toAssemblyLabel label

toAssemblyLabel = show
