module LMLEval where

import Control.Monad (guard)

import LMLExpr
import LMLParser
import Parser


lmlGenCompoundType :: String -> [[String]] -> LMLEnv -> Maybe LMLType
lmlGenCompoundType tname (a:as) env = LMLCompoundType tname <$> (sequence $ map sequence $ go (a:as))
    where
        go []     = []
        go (x:xs) = (map (fmap (detType . fst)) $ (map (runParser lmlTypeParser) x)) : go xs

        detType t@(LMLTrivialType name)
            | name == tname = LMLSelfType name
        detType t = lmlDetermineType t env



lmlGenTypeCTor :: LMLType -> String  -> [LMLType] -> LMLValue
lmlGenTypeCTor rettype name []                                       = LMLValueCompound (LMLCompound rettype name [])
lmlGenTypeCTor rettype@(LMLCompoundType rettypename _) name argtypes = LMLValueFunc $ LMLFunction rettype argtypes $ \env args -> do
    guard $ and $ map (uncurry (==)) $ zip (map collapseSelfType argtypes) (map lmlGetType args)
    Just (env, LMLValueCompound (LMLCompound rettype name args))

    where
        collapseSelfType (LMLSelfType t)
            | t == rettypename = rettype
        collapseSelfType t = t



lmlSequencedEval :: LMLEnv -> [LMLAst] -> Maybe (LMLEnv, [LMLValue])
lmlSequencedEval env = foldl f (Just (env, []))
    where
        f Nothing _ = Nothing
        f (Just (env', vals)) next = do
            (env'', val) <- lmlEval env' next
            Just (env'', vals ++ [val])



lmlCurry :: LMLFunction -> [LMLValue] -> LMLFunction
lmlCurry (LMLFunction rt at f) curriedArgs = LMLFunction rt (drop (length curriedArgs) at) $ \env args -> f env (curriedArgs ++ args)



lmlEval :: LMLEnv -> LMLAst -> Maybe (LMLEnv, LMLValue)
lmlEval env (LMLAstSeq (x:xs)) = case lmlEval env x of
    Just (env', LMLValueFunc func@(LMLFunction rettype argtypes f)) -> do
        (env'', args) <- lmlSequencedEval env' xs

        -- todo figure out how to typecheck here (complication: SelfTypes), i may have to typecheck inside the function

        if length args < length argtypes
            then Just (env'', LMLValueFunc $ lmlCurry func args)
            else f env'' args
        
    _ -> error "Expected function call"

lmlEval env (LMLAstBlock list) = case lmlSequencedEval env list of
    Just (env', [])   -> Just (env', LMLValueNil)
    Just (env', vals) -> Just (env', last vals)
    _                 -> Nothing

lmlEval env (LMLAstList list) = case lmlSequencedEval env list of
    Just (env', [])     -> Just $ (env', LMLValueList (LMLList (LMLTrivialType "") []))
    Just (env', (x:xs)) -> if and $ map (== lmlGetType x) (map lmlGetType xs)
        then Just $ (env', LMLValueList (LMLList (lmlGetType x) (x:xs)))
        else error "non homogeneous list found"

    Nothing -> Nothing

lmlEval env (LMLAstLetExpr id expr) = do
    (env', evaledval) <- lmlEval env expr
    Just $ (lmlEnvInsertData id evaledval env', LMLValueNil)

lmlEval env (LMLAstLambda ids expr) = Just (env, LMLValueFunc (LMLFunction (LMLTrivialType "") (replicate (length ids) (LMLTrivialType "")) (f ids)))
    where
        f :: [String] -> LMLEnv -> [LMLValue] -> Maybe (LMLEnv, LMLValue)
        f ids = \env params -> do
            (_, res) <- lmlEval (lmlEnvInsertAll (zip ids params) env) expr
            Just (env, res)

lmlEval env (LMLAstDataDecl tname ctors) = do
    newt@(LMLCompoundType _ alts) <- lmlGenCompoundType tname (map snd ctors) env
    
    let env' = lmlEnvInsertType tname newt env
    let ctorNames = map fst ctors
    let allCTors = map (uncurry (lmlGenTypeCTor newt)) (zip ctorNames alts)
    let env'' = lmlEnvInsertAll (zip ctorNames allCTors) env'

    Just (env'', LMLValueNil)


lmlEval env (LMLAstIdent i) = (\d -> (env, d)) <$> lmlEnvlookupData i env

lmlEval env (LMLAstValue v) = Just (env, v)


{-

lispEval env (LispFunExpr (id, paramids, expr)) = do
    (env', func) <- lispEval env (LispLambdaExpr (paramids, expr))
    Just (insertData id func env', LispNil)

lispEval env (LispInfixExpr (op, a, b)) = do
    (LispFun f) <- lookupData op env
    res <- f env [a, b]
    Just (env, res)


lispEval env (LispEnumDecl (ident, alts)) = Just (lispEnvInsertAll (genTypeConstructors (ident, alts)) (lmlEnvInserType (LispTypeTrivial ident) env), LispNil)

lispEval (dat, types) (LispIdent id) = (\val -> ((dat, types), val)) <$> Map.lookup id dat
-}



