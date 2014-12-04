
module Language.Malfoy.Syntax where

type Ident = String

data Program = Program [(Ident, Expr)]
    deriving (Show, Eq)

data Expr = Const Integer
          | Var Ident
          | BinOp BinOp Expr Expr
          | Let [(Ident, Expr)] Expr
          | Lambda [Ident] Expr
          | Apply Expr [Expr]
          | If Expr Expr Expr
    deriving (Show, Eq)

data BinOp = Add | Sub | Mul | Div | And | Equals | NotEquals | LessThan | GreaterThan | LessThanOrEqual | GreaterThanOrEqual
    deriving (Show, Eq)

