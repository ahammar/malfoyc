module MalfoyCompiler.Optimizer (
    optimize
) where

import Data.Map as M

import MalfoyCompiler.Graph
import MalfoyCompiler.Opcodes

optimize :: Graph -> Graph
optimize graph = case foldConstant graph of
    Changed graph -> optimize graph
    Unchanged -> graph

data Optimization = Changed Graph | Unchanged

addConstant const target =
    case target of
        Just (_, label) -> addConstant' const label
        Nothing -> id
  where  
    addConstant' const label = update (setConstant const) label
    setConstant const (Instruction opcode t0 t1 Nothing)
        = Just $ Instruction opcode t0 t1 (Just const)
    setConstant const _ = error "Not implemented yet"

foldConstant :: Graph -> Optimization
foldConstant graph = go (M.toList graph)
  where
    go [] = Unchanged
    go ((lbl, (Instruction Dup t0 t1 (Just const))):_) =
        Changed (M.delete lbl . addConstant const t0 . addConstant const t1 $ graph) 
    go (_:rest) = go rest
