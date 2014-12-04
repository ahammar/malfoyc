
module MalfoyCompiler.Simplifier where

import Control.Arrow

import Language.Malfoy.Syntax
    
simplify :: Program -> Program

simplify (Program decls) = Program (map (second simplifyExpr) decls)


simplifyExpr :: Expr -> Expr

simplifyExpr (BinOp op (Const lhs) (Const rhs)) =
    Const $ case op of
        Add -> lhs + rhs
        Sub -> lhs - rhs
        Mul -> lhs * rhs
        Div -> lhs `div` rhs
        _ -> error "Not implemented"

simplifyExpr other = other
