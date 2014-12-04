
{
module Language.Malfoy.Lexer (Token(..), lexer) where
import Language.Malfoy.Syntax
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
    $white+                 ;
    \-\-[^\n]*              ;
    "=="                    { \s -> TBinOp Equals }
    "!="                    { \s -> TBinOp NotEquals }
    "="                     { \s -> TEquals }
    "+"                     { \s -> TBinOp Add }
    "-"                     { \s -> TBinOp Sub }
    "*"                     { \s -> TBinOp Mul }
    "/"                     { \s -> TBinOp Div }
    "&"                     { \s -> TBinOp And }
    "("                     { \s -> TLeftParen }
    ")"                     { \s -> TRightParen }
    "->"                    { \s -> TRightArrow }
    ","                     { \s -> TComma }
    "<"                     { \s -> TBinOp LessThan }
    ">"                     { \s -> TBinOp GreaterThan }
    "<="                    { \s -> TBinOp LessThanOrEqual }
    ">="                    { \s -> TBinOp GreaterThanOrEqual }
    "lambda"                { \s -> TLambda }
    "if"                    { \s -> TIf }
    "then"                  { \s -> TThen }
    "else"                  { \s -> TElse }
    "let"                   { \s -> TLet }
    "in"                    { \s -> TIn }
    ";"                     { \s -> TSep }
    $digit+                 { \s -> TInt (read s) }
    $alpha[$alpha $digit]*  { \s -> TIdent s }

{

data Token = TIdent String
           | TInt Integer
           | TEquals
           | TBinOp BinOp
           | TLeftParen
           | TRightParen
           | TRightArrow
           | TLambda
           | TIf
           | TThen
           | TElse
           | TLet
           | TIn
           | TComma
           | TSep
    deriving (Show)

lexer = alexScanTokens
}
