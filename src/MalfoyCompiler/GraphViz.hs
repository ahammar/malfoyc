
module MalfoyCompiler.GraphViz (toGraphViz) where

import qualified Data.Map as M

import MalfoyCompiler.Graph

toGraphViz :: Graph -> String
toGraphViz graph = "digraph { node [shape=\"record\"]\n " ++ unlines (nodes ++ edges)  ++ "}"
    where nodes = map toNode $ M.toList graph
          edges = concatMap toEdges $ M.toList graph

toNode :: (Label, Instruction) -> String
toNode (label, Instruction opcode t0 t1 const) =
    show label ++ "[label=\"{" ++ show label ++ "|" ++ bottom ++ "}\"]"
  where
    bottom = case const of
        Just (Literal x) -> "{" ++ show opcode ++ "|$" ++ show x ++ "}"
        Just (Pointer x) -> "{" ++ show opcode ++ "|&" ++ show x ++ "}"
        Nothing -> show opcode

toEdges :: (Label, Instruction) -> [String]
toEdges (label, Instruction _ t0 t1 _) = first ++ second
  where
    first = case t0 of
        Just (Lhs, lbl) -> edge "L" lbl "L"
        Just (Rhs, lbl) -> edge "L" lbl "R"
        Nothing -> []

    second = case t1 of
        Just (Lhs, lbl) -> edge "R" lbl "L"
        Just (Rhs, lbl) -> edge "R" lbl "R"
        Nothing -> []

    edge out target side = [show label ++ " -> " ++ show target ++ "[taillabel=\"" ++ out ++ "\", headlabel=\"" ++ side ++ "\"]"]
