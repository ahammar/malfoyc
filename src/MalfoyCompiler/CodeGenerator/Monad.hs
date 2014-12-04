
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

module MalfoyCompiler.CodeGenerator.Monad (CodeGen, runCodeGen, emitInstruction, emitConstant, putInstruction, label) where

import Control.Monad.Error
import Control.Monad.State
import Data.List (foldl')
import Data.Map (Map, fromList, toList, insert, empty, adjust)
import qualified Data.Map as M

import Language.Malfoy.Syntax (Ident)
import MalfoyCompiler.Graph
import MalfoyCompiler.Error
import MalfoyCompiler.Opcodes

newtype CodeGen a = CodeGen (StateT CodeGenState (Either CompileError) a)
    deriving (Functor, Monad, MonadError CompileError)

data CodeGenState
    = CGS { next :: Label, code :: Graph, constants :: [(Constant, Label)] }

-- | Runs code generation, returning the generated instruction graph.
runCodeGen :: CodeGen a -> ThrowsError Graph
runCodeGen (CodeGen m) = do
    s <- execStateT m freshState
    return $ foldl' apply (code s) (constants s)

    where freshState = CGS { next = minBound, code = empty, constants = [] }
          apply :: Graph -> (Constant, Label) -> Graph
          apply code (c, l) = adjust (setConstant c) l code

          setConstant :: Constant -> Instruction -> Instruction
          setConstant c (Instruction opcode t0 t1 Nothing) = Instruction opcode t0 t1 (Just c)

-- | Emits a constant.
emitConstant :: Constant -> Target -> CodeGen ()
emitConstant constant target = CodeGen $ case target of
        (Just (_, label)) -> modify (\state -> state { constants = (constant, label) : constants state })
        Nothing -> return ()

-- | Emits an instruction.
emitInstruction :: Opcode -> Target -> Target -> CodeGen Label
emitInstruction opcode t0 t1 = do
    name <- label
    putInstruction name opcode t0 t1
    return name

-- | Store an instruction
putInstruction :: Label -> Opcode -> Target -> Target -> CodeGen ()
putInstruction name opcode t0 t1 = do 
    CodeGen $ modify (\state -> state { code = insert name (Instruction opcode t0 t1 Nothing) (code state) })

-- FIXME: Needs cleanup and renaming.
mapM' :: (Monad m, Ord k) => (a -> m b) -> Map k a -> m (Map k b)
mapM' f = liftM fromList . mapM (\(x,y) -> do y' <- f y; return (x, y')) . toList

-- | Create a new unique label.
label :: CodeGen Label
label = CodeGen $ StateT (\s -> return (next s, s { next = succ (next s) }))

