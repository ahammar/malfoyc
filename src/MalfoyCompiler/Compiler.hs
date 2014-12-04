
module MalfoyCompiler.Compiler (compile) where

import Control.Monad

import Language.Malfoy.Parser
import MalfoyCompiler.CodeGenerator
import MalfoyCompiler.Error
import MalfoyCompiler.Graph
import MalfoyCompiler.Optimizer
import MalfoyCompiler.Simplifier

compile :: String -> Either CompileError Graph
compile = liftM optimize . generate . simplify . parse
