
module MalfoyCompiler.CodeGenerator (generate) where

import Control.Arrow
import Control.Monad
import Control.Monad.Error
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

import Language.Malfoy.Syntax
import MalfoyCompiler.CodeGenerator.Monad
import MalfoyCompiler.Error
import MalfoyCompiler.Graph
import qualified MalfoyCompiler.Opcodes as Op

-- | Generate a program graph from the abstract syntax tree.
generate :: Program -> ThrowsError Graph
generate (Program decls) = runCodeGen $ do
    -- Needs insertion of Seqs
    args <- case lookup "main" decls of
        Just (Lambda args _) -> return args
        Just _  -> fail "main must be a lambda expression"
        Nothing -> throwError (UndefinedReference "main")

    let (Lambda args _) = fromJust $ lookup "main" decls
        

    let numArgs = length $ args

    let distribute []     = return (nowhere, [])
        distribute (a:as) = do
            (next, nodes) <- distribute as
            argNode <- emitInstruction Op.Arg next nowhere
            return (lhs argNode, rhs argNode : nodes)

    (args', nodes) <- distribute args

    exit <- emitInstruction Op.Exit nowhere nowhere

    call <- emitInstruction Op.Call (lhs exit) args'

    let distribute [t] = fmap lhs $ emitInstruction Op.Seq (lhs call) t
        distribute (t:ts) = do
            t' <- distribute ts
            fmap lhs $ emitInstruction Op.Seq t' t

    top <- distribute nodes

    fvs <- generateExpr (Let decls (Var "main")) top

    when (not $ M.null fvs) (fail "Free variables at top level")

type FreeVars = M.Map Ident Target

unifyWith :: (Monad m, Ord k) => (Maybe a -> Maybe b -> m c) -> M.Map k a -> M.Map k b -> m (M.Map k c)
unifyWith g f0 f1 = do
    let f = M.keysSet f0 `S.union` M.keysSet f1
    ps <- forM (S.toList f) $ \var -> do
        x <- g (M.lookup var f0) (M.lookup var f1)
        return (var, x)
    return $ M.fromList ps

unify :: FreeVars -> FreeVars -> CodeGen FreeVars
unify = unifyWith $ \t0 t1 ->
    case (t0, t1) of
        (Just t0, Just t1) -> fmap lhs $ emitInstruction Op.Dup t0 t1
        (Just t0, Nothing) -> return t0
        (Nothing, Just t1) -> return t1

unifyWithBranches :: FreeVars -> FreeVars -> CodeGen (M.Map Ident Label)
unifyWithBranches = unifyWith $ \true false -> do
    emitInstruction Op.Branch (maybeNowhere true) (maybeNowhere false)
    where maybeNowhere = fromMaybe nowhere

generateExpr :: Expr -> Target -> CodeGen FreeVars

generateExpr (Const value) target = do
    emitConstant (Literal value) target
    return M.empty

generateExpr (Var var) target =
    return (M.singleton var target)

generateExpr (BinOp op x y) target = do
    operator <- emitOperator op target

    fx <- generateExpr x (lhs operator)
    fy <- generateExpr y (rhs operator)

    unify fx fy

generateExpr (Let bindings body) target = do
    labels <- mapM (\x -> liftM ((,) x) label) bindings

    fs <- forM labels $ \((_, expr), label) -> do
        generateExpr expr (lhs label)

    fb <- generateExpr body target
    
    fvs <- foldM unify M.empty (fb:fs)

    forM_ labels $ \((name, _), label) -> do
        let (Just target) = M.lookup name fvs
        putInstruction label Op.Dup target nowhere
    
    return (foldl (flip M.delete) fb (map fst bindings))

generateExpr (Lambda vars body) target = do
    ret    <- emitInstruction Op.Ret nowhere nowhere
    fb     <- generateExpr body (rhs ret)
    params <- distribute fb vars

    label <- emitInstruction Op.Context (lhs ret) params
    emitConstant (Pointer label) target

    return (foldl (flip M.delete) fb vars)
  where
    distribute db [] = return nowhere
    distribute fb (a:as) = do
        next <- distribute fb as
        let target = fromMaybe nowhere $ M.lookup a fb 
        fmap lhs $ emitInstruction Op.Param target next

generateExpr (Apply fun args) target = do
    let distribute []     = return (nowhere, [])
        distribute (a:as) = do
            (next, nodes) <- distribute as
            argNode <- emitInstruction Op.Arg next nowhere
            return (lhs argNode, rhs argNode : nodes)

    (args', nodes) <- distribute args

    call <- emitInstruction Op.Call target args'

    let distribute [] = return (lhs call, [])
        distribute (t:ts) = do
            (next, nodes) <- distribute ts
            lbl <- emitInstruction Op.Seq next t
            return (lhs lbl, rhs lbl : nodes)

    (top, nodes') <- distribute nodes

    fvs <- foldM (\fv (expr, target) -> do f <- generateExpr expr target; unify f fv) M.empty (zip args nodes')
        

    ff <- generateExpr fun top

    unify ff fvs
  where distribute [] = return (nowhere, M.empty)
        distribute (a:as) = do
            (arg, fvs) <- distribute as
            lbl <- emitInstruction Op.Arg arg nowhere
            fvs' <- generateExpr a (rhs lbl)
            fvs'' <- unify fvs fvs'
            return (lhs lbl, fvs'')


generateExpr (If cond true false) target = do
    ft <- generateExpr true target
    ff <- generateExpr false target

    -- figure out the union of free vars and
    -- make branch instructions
    bs <- unifyWithBranches ft ff

    bt <- distribute (M.elems $ M.map lhs bs)
    fc <- generateExpr cond bt

    let fu = M.map rhs bs
    unify fc fu

  where
    distribute [t] = return t 
    distribute (t:ts) = do
        t' <- distribute ts
        fmap lhs $ emitInstruction Op.Dup t t'

emitOperator :: BinOp -> Target -> CodeGen Label

emitOperator NotEquals target = invert target >>= emitOperator Equals
emitOperator GreaterThan target = emitOperator LessThan target >>= swapInputs
emitOperator LessThanOrEqual target = invert target >>= emitOperator GreaterThan
emitOperator GreaterThanOrEqual target = invert target >>= emitOperator LessThan

emitOperator op target = emitInstruction opcode target nowhere
    where opcode = case op of
              Add      -> Op.Add
              Sub      -> Op.Sub
              Mul      -> Op.Mul
              Div      -> Op.Div
              And      -> Op.And
              Equals   -> Op.Equals
              LessThan -> Op.LessThan

swapInputs :: Label -> CodeGen Label
swapInputs lbl = emitInstruction Op.Seq (rhs lbl) (lhs lbl)

invert :: Target -> CodeGen Target
invert target = fmap lhs $ emitInstruction Op.Not target nowhere

