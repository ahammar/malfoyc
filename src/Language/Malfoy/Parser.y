{
module Language.Malfoy.Parser (parse) where

import Language.Malfoy.Lexer
import Language.Malfoy.Syntax
}

%name parseTokens
%tokentype { Token }
%error { parseError }

%token
    ident               { TIdent $$ }
    integer             { TInt $$ }
    "="                 { TEquals }
    "+"                 { TBinOp Add }
    "-"                 { TBinOp Sub }
    "*"                 { TBinOp Mul }
    "/"                 { TBinOp Div }
    "&"                 { TBinOp And }
    "=="                { TBinOp Equals }
    "!="                { TBinOp NotEquals }
    "<"                 { TBinOp LessThan }
    ">"                 { TBinOp GreaterThan }
    "<="                { TBinOp LessThanOrEqual }
    ">="                { TBinOp GreaterThanOrEqual }
    "("                 { TLeftParen }
    ")"                 { TRightParen }
    "->"                { TRightArrow }
    ","                 { TComma }
    lambda              { TLambda }
    if                  { TIf }
    then                { TThen }
    else                { TElse }
    let                 { TLet }
    in                  { TIn }
    ";"                 { TSep }

%nonassoc if then else
%right lambda "->" let in
%left "&"
%nonassoc "==" "!=" "<" ">" "<=" ">="
%left "+" "-"
%left "*" "/"
%nonassoc APPLY

%%

Program : DeclList              { Program $1 }

DeclList : Decl ";"             { [$1] }
         | Decl ";" DeclList    { $1 : $3 }

Decl : ident "=" Exp            { ($1, $3) }

Exp : Exp "+" Exp               { BinOp Add $1 $3 }
    | Exp "-" Exp               { BinOp Sub $1 $3 }
    | Exp "*" Exp               { BinOp Mul $1 $3 }
    | Exp "/" Exp               { BinOp Div $1 $3 }
    | Exp "&" Exp               { BinOp And $1 $3 }
    | Exp "==" Exp              { BinOp Equals $1 $3 }
    | Exp "!=" Exp              { BinOp NotEquals $1 $3 }
    | Exp "<" Exp               { BinOp LessThan $1 $3 }
    | Exp ">" Exp               { BinOp GreaterThan $1 $3 }
    | Exp "<=" Exp              { BinOp LessThanOrEqual $1 $3 }
    | Exp ">=" Exp              { BinOp GreaterThanOrEqual $1 $3 }
    | ident                     { Var $1 }
    | integer                   { Const $1 }
    | lambda Params "->" Exp    { Lambda $2 $4 }
    | let Bindings in Exp       { Let $2 $4 }
    | Exp Args %prec APPLY      { Apply $1 $2 }
    | if Exp then Exp else Exp  { If $2 $4 $6 }
    | "(" Exp ")"               { $2 }

Params : ident                  { [$1] }
       | ident Params           { $1 : $2 }

Args : Exp %prec APPLY          { [$1] }
     | Exp Args %prec APPLY     { $1 : $2 }

Bindings : Binding              { [$1] }
         | Binding "," Bindings { $1 : $3 }

Binding : ident "=" Exp         { ($1, $3) }

{
parse = parseTokens . lexer

parseError s = error ("Parse error" ++ show s)
}
