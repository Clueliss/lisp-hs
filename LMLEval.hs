module LMLEval where

import Control.Monad (guard)
import Text.Printf

import LMLExpr
import LMLParser
import Parser


lmlTypecheck :: LMLType -> LMLValue -> Bool
lmlTypecheck t v = lmlTypecheck' t (lmlGetType v)
    where
        lmlTypecheck' (LMLFunctionType lrettype largtypes) (LMLFunctionType rrettype rargtypes) 
            = (lmlTypecheck' lrettype rrettype) && (and $ map (uncurry lmlTypecheck') (zip largtypes rargtypes))

        lmlTypecheck' (LMLTrivialType "") _ = True
        lmlTypecheck' (LMLTrivialType l) (LMLTrivialType r)
            | l == r = True

        lmlTypecheck' et vt 
            | et == vt  = True
            | otherwise = error (printf "typecheck failed: expected type: %s got: %s instead" (show et) (show vt))


lmlGenCompoundType :: String -> [[String]] -> LMLEnv -> Maybe LMLType
lmlGenCompoundType tname (a:as) env = LMLCompoundType tname <$> (sequence $ map sequence $ go (a:as))
    where
        go []     = []
        go (x:xs) = (map (fmap (detType . fst)) $ (map (runParser lmlTypeParser) x)) : go xs

        detType t@(LMLTrivialType name)
            | name == tname = LMLSelfType name
        detType t = lmlDetermineType env t



lmlGenTypeCTor :: LMLType -> String  -> [LMLType] -> LMLValue
lmlGenTypeCTor rettype name []                                       = LMLValueCompound (LMLCompound rettype name [])
lmlGenTypeCTor rettype@(LMLCompoundType rettypename _) name argtypes = LMLValueFunc $ LMLFunction rettype argtypes $ \env args -> do
    guard $ and $ map (uncurry lmlTypecheck) $ zip (map collapseSelfType argtypes) args
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
lmlEval env (LMLAstSeq []) = Just (env, LMLValueNil)
lmlEval env (LMLAstSeq (x:xs)) = case lmlEval env x of
    Just (env', LMLValueFunc func@(LMLFunction rettype argtypes f)) -> do
        
        (env'', args) <- lmlSequencedEval env' xs

        case compare (length args) (length argtypes) of
            GT -> error (printf "supplied too many args to function expected: %d got: %d" (length args) (length argtypes))
            LT -> Just (env'', LMLValueFunc $ lmlCurry func args)
            EQ -> f env'' args

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
    Just (lmlEnvInsertData id evaledval env', LMLValueNil)

lmlEval env (LMLAstDataDecl tname ctors) = do
    newt@(LMLCompoundType _ alts) <- lmlGenCompoundType tname (map snd ctors) env
    
    let env'      = lmlEnvInsertType tname newt env
    let ctorNames = map fst ctors
    let allCTors  = map (uncurry (lmlGenTypeCTor newt)) (zip ctorNames alts)
    let env''     = lmlEnvInsertAll (zip ctorNames allCTors) env'

    Just (env'', LMLValueNil)

lmlEval env (LMLAstLambda ids expr) = Just (env, LMLValueFunc (LMLFunction rettype argtypes f))
    where
        rettype  = LMLTrivialType ""
        argtypes = replicate (length ids) (LMLTrivialType "")

        f :: LMLEnv -> [LMLValue] -> Maybe (LMLEnv, LMLValue)
        f env params = do
            (_, res) <- lmlEval (lmlEnvInsertAll (zip ids params) env) expr
            Just (env, res)

lmlEval env (LMLAstFunction name rettype args body) = Just (lmlEnvInsertData name (LMLValueFunc (LMLFunction rettype argtypes f)) env, LMLValueNil)
    where
        expectedRettype = lmlDetermineType env rettype
        argtypes        = map (lmlDetermineType env . snd) args
        argnames        = map fst args

        f :: LMLEnv -> [LMLValue] -> Maybe (LMLEnv, LMLValue)
        f env params = do
            guard (and $ map (uncurry lmlTypecheck) (zip argtypes params))

            (_, res) <- lmlEval (lmlEnvInsertAll (zip argnames params) env) body

            guard (lmlTypecheck expectedRettype res)
            Just (env, res)


lmlEval env (LMLAstIdent i) = ((,) env) <$> lmlEnvLookupData i env
lmlEval env (LMLAstValue v) = Just (env, v)
