module LMLEval where

import Control.Monad (guard)
import qualified Data.Map.Strict as Map
import Text.Printf

import LMLExpr
import LMLParser
import Parser


lmlTypecheck :: Map.Map String LMLType -> LMLType -> LMLValue -> (Map.Map String LMLType, Bool)
lmlTypecheck genericIdx t v = lmlTypecheck' genericIdx t (lmlGetType v)
    where
        lmlTypecheck' :: Map.Map String LMLType -> LMLType -> LMLType -> (Map.Map String LMLType, Bool)

        lmlTypecheck' gIndex (LMLGenericType gt) (LMLTrivialType "") = (gIndex, True)
        lmlTypecheck' gIndex (LMLGenericType gt) t 
            | gt `Map.member` gIndex = (gIndex,                 (gIndex Map.! gt) == t)
            | otherwise              = (Map.insert gt t gIndex, True)

        lmlTypecheck' gIndex (LMLFunctionType lrettype largtypes) (LMLFunctionType rrettype rargtypes)
            = (gIndex'', retMatch && argMatch)

            where
                typefold (gIdx, lastValid) (l, r) = let (gIdx', thisValid) = lmlTypecheck' gIdx l r
                    in (gIdx', thisValid && lastValid) 

                (gIndex',  retMatch) = lmlTypecheck' gIndex lrettype rrettype
                (gIndex'', argMatch) = foldl typefold (gIndex', True) (zip largtypes rargtypes)


        lmlTypecheck' gIndex (LMLTrivialType "") _ = (gIndex, True)
        lmlTypecheck' gIndex (LMLTrivialType l) (LMLTrivialType r)
            | l == r = (gIndex, True)

        lmlTypecheck' gIndex et vt 
            | et == vt  = (gIndex, True)
            | otherwise = error (printf "typecheck failed: expected type: %s got: %s instead" (show et) (show vt))


sequencedTypecheck :: Map.Map String LMLType -> [(LMLType, LMLValue)] -> (Map.Map String LMLType, Bool)
sequencedTypecheck genericIndex tv = sequencedTypecheck' True genericIndex tv
    where
        sequencedTypecheck' valid gIdx []         = (gIdx, valid)
        sequencedTypecheck' valid gIdx ((l,r):xs) = let (gIdx', valid') = lmlTypecheck gIdx l r
            in sequencedTypecheck' (valid && valid') gIdx' xs




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
    guard $ and $ map (snd . uncurry (lmlTypecheck (Map.fromList []))) $ zip (map collapseSelfType argtypes) args
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

lmlEval env (LMLAstLambda captures ids expr) = Just (env, LMLValueFunc (LMLFunction rettype argtypes f))
    where
        capturedVals = sequence $ map (flip lmlEnvLookupData env) captures
        rettype  = LMLTrivialType ""
        argtypes = replicate (length ids) (LMLTrivialType "")

        f :: LMLEnv -> [LMLValue] -> Maybe (LMLEnv, LMLValue)
        f env params = do
            capVals <- capturedVals
            let env' = lmlEnvInsertAll (zip captures capVals) env

            -- hier typecheck

            (_, res) <- lmlEval (lmlEnvInsertAll (zip ids params) env') expr
            Just (env, res)

lmlEval env (LMLAstFunction name rettype args body) = Just (lmlEnvInsertData name (LMLValueFunc (LMLFunction rettype argtypes f)) env, LMLValueNil)
    where
        expectedRettype = lmlDetermineType env rettype
        argtypes        = map (lmlDetermineType env . snd) args
        argnames        = map fst args

        resolveGenerics :: Map.Map String LMLType -> LMLType -> LMLType
        resolveGenerics genericIdx (LMLGenericType gt)        = genericIdx Map.! gt
        resolveGenerics genericIdx (LMLListType t)            = LMLListType $ resolveGenerics genericIdx t
        resolveGenerics genericIdx (LMLFunctionType ret args) = LMLFunctionType (resolveGenerics genericIdx ret) (map (resolveGenerics genericIdx) args)
        resolveGenerics _          t                          = t

        f :: LMLEnv -> [LMLValue] -> Maybe (LMLEnv, LMLValue)
        f localEnv params = do
            let (gIdx, argTypesOk) = sequencedTypecheck (Map.fromList []) (zip argtypes params)
            guard argTypesOk

            (_, res) <- lmlEval (lmlEnvInsertAll (zip argnames params) localEnv) body

            let (gIdx', retTypeOk) = lmlTypecheck gIdx expectedRettype res
            guard retTypeOk

            case res of
                LMLValueFunc (LMLFunction _ _ f) -> let LMLFunctionType eRetT eArgT = resolveGenerics gIdx' expectedRettype
                    in Just (env, LMLValueFunc (LMLFunction eRetT eArgT f))
                other                                  -> Just (env, other) 


lmlEval env (LMLAstIdent i) = ((,) env) <$> lmlEnvLookupData i env
lmlEval env (LMLAstValue v) = Just (env, v)
