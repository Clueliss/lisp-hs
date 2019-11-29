module LispEval where

import Data.Map as Map (lookup, insert)
import Data.Maybe (fromJust)

import LispExpr
import MathExpr



lispEvalInSequence :: LispEnv -> [LispExpr] -> Maybe (LispEnv, [LispExpr])
lispEvalInSequence env = foldl f (Just (env, []))
    where
        f :: Maybe (LispEnv, [LispExpr]) -> LispExpr -> Maybe (LispEnv, [LispExpr])
        f Nothing _ = Nothing
        f (Just (env', vals)) next = do
            (env'', val) <- lispEval env' next
            Just (env'', vals ++ [val])



lispEval :: LispEnv -> LispExpr -> Maybe (LispEnv, LispExpr)
lispEval env (LispSeq (x:xs)) = f (lispEval env x)
    where
        f :: Maybe (LispEnv, LispExpr) -> Maybe (LispEnv, LispExpr)
        
        f (Just (env', LispIntrinsicFun g)) = g env' xs
        
        f (Just (env', LispFun h)) = do
            (env'', vals) <- lispEvalInSequence env' xs
            res <- h env'' vals

            Just (env'', res)

        f _ = Nothing

lispEval env (LispBlock list) = f (lispEvalInSequence env list)
    where
        f :: Maybe (LispEnv, [LispExpr]) -> Maybe (LispEnv, LispExpr)
        f (Just (env', [])) = Just (env', LispNil)
        f (Just (env', vals)) = Just (env', last vals)
        f _ = Nothing

lispEval env (LispList list) = (\(env', list') -> (env', LispList list')) 
    <$> lispEvalInSequence env list

lispEval env (LispLetExpr (id, val, expr)) = do
    (env', evaledval) <- lispEval env val
    lispEval (insert id evaledval env') expr

lispEval env (LispLambdaExpr (paramids, bodyexpr)) = Just (env, LispFun $ f paramids)
    where
        f ids = \env params -> do
            (env', vals) <- lispEvalInSequence env params
            (_, res) <- lispEval (lispEnvInsertAll (zip ids vals) env') bodyexpr
            Just res

lispEval env (LispFunExpr (id, paramids, expr)) = do
    (env', func) <- lispEval env (LispLambdaExpr (paramids, expr))
    Just ((insert id func env'), LispNil)

lispEval env (LispInfixExpr (op, a, b)) = do
    (env', LispNum evala) <- lispEval env a
    (env'', LispNum evalb) <- lispEval env' b

    (\res -> (env, LispNum res)) <$> evalMathExpr (MathExprOp (op, MathExprNum evala, MathExprNum evalb))

lispEval env (LispIdent id) = (\val -> (env, val)) <$> Map.lookup id env
lispEval env val = Just (env, val)
