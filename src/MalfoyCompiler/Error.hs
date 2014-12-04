
module MalfoyCompiler.Error where

import Control.Monad.Error

import Language.Malfoy.Syntax

data CompileError = UndefinedReference Ident
                  | UnknownError String
    deriving (Show)

instance Error CompileError where
    strMsg = UnknownError

type ThrowsError a = Either CompileError a

