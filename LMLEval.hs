module LMLEval where

import Control.Monad (guard)
import qualified Data.Map.Strict as Map
import Text.Printf

import LMLExpr
import LMLParser
import LMLType
import Parser
import Util


lmlGenCompoundType :: String -> [[String]] -> LMLEnv -> Either String [[LMLType]]
lmlGenCompoundType tname (a:as) env = fromJustOrErr "" $ sequence $ map sequence $ go (a:as)
    where
        go []     = []
        go (x:xs) = (map (fmap (detType . fst)) $ (map (runParser lmlTypeParser) x)) : go xs

        detType t@(LMLTrivialType name)
            | name == tname = LMLSelfType name
        detType t = lmlDetermineType env t


lmlGenTypeCTor :: LMLType -> String  -> [LMLType] -> LMLValue
lmlGenTypeCTor rettype name []                                       = LMLValueCompound (LMLCompound rettype name [])
lmlGenTypeCTor rettype@(LMLCompoundType rettypename _) name argtypes = LMLValueFunc $ LMLFunction rettype argtypes $ \env args -> do
    -- PROBABLY USELESS guard $ and $ map (snd . uncurry (lmlTypecheck (Map.fromList []))) $ zip (map collapseSelfType argtypes) args
    Right (env, LMLValueCompound (LMLCompound rettype name args))


lmlCurry :: LMLFunction -> [LMLValue] -> LMLFunction
lmlCurry (LMLFunction rt at f) curriedArgs = LMLFunction rt (drop (length curriedArgs) at) $ \env args -> f env (curriedArgs ++ args)


lmlSequencedEval :: LMLEnv -> [LMLAst] -> Either String (LMLEnv, [LMLValue])
lmlSequencedEval env = foldl f (Right (env, []))
    where
        f (Left e) _ = (Left e)
        f (Right (env', vals)) next = do
            (env'', val) <- lmlEval env' next
            Right (env'', vals ++ [val])


lmlEval :: LMLEnv -> LMLAst -> Either String (LMLEnv, LMLValue)
lmlEval env (LMLAstSeq [])     = Right (env, LMLValueNil)
lmlEval env (LMLAstSeq (x:xs)) = case lmlEval env x of
    Right (env', LMLValueFunc func@(LMLFunction rettype argtypes f)) -> do
        (env'', args) <- lmlSequencedEval env' xs

        let (gIdx, argsOk) = sequencedTypecheck (Map.fromList []) (zip expectedArgtypes args)
        guardWithErr "Arguments to functioncall did not typecheck" argsOk

        case compare (length args) (length argtypes) of
            GT -> Left (printf "supplied too many args to function expected: %d got: %d" (length args) (length argtypes))
            LT -> Right (env'', LMLValueFunc $ lmlCurry func args)
            EQ -> do
                (env''', res) <- f env'' args
                
                let resolvedRettype = lmlResolveGenerics gIdx rettype
                let res' = lmlAssignFixedType gIdx resolvedRettype res -- noop if not a lambda
                
                let (gIdx', retOk) = lmlTypecheck gIdx resolvedRettype res'
                guardWithErr "Return value of function call did not typecheck" retOk

                Right (env''', res')
        where
            expectedArgtypes = map (lmlDetermineType env) argtypes
            expectedRettype  = lmlDetermineType env rettype

    Right (_, v) -> Left (printf "Tried to call %s which is not a function" (show v))
    Left e       -> Left e

lmlEval env (LMLAstBlock list) = case lmlSequencedEval env list of
    Right (env', [])   -> Right (env', LMLValueNil)
    Right (env', vals) -> Right (env', last vals)
    Left e             -> Left e

lmlEval env (LMLAstList list) = case lmlSequencedEval env list of
    Right (env', [])     -> Right $ (env', LMLValueList (LMLList (LMLTrivialType "") []))
    Right (env', (x:xs)) -> if and $ map (== lmlGetType x) (map lmlGetType xs)
        then Right (env', LMLValueList (LMLList (lmlGetType x) (x:xs)))
        else Left "non homogeneous list found"

    Left e -> Left e

lmlEval env (LMLAstLetExpr id expr) = do
    (env', evaledval) <- lmlEval env expr
    Right (lmlEnvInsertData id evaledval env', LMLValueNil)

lmlEval env (LMLAstDataDecl tname ctors) = do
    ctorTypes <- lmlGenCompoundType tname (map snd ctors) env

    let newt = LMLCompoundType tname ctorTypes
    
    let env'      = lmlEnvInsertType tname newt env
    let ctorNames = map fst ctors
    let allCTors  = map (uncurry (lmlGenTypeCTor newt)) (zip ctorNames ctorTypes)
    let env''     = lmlEnvInsertAll (zip ctorNames allCTors) env'

    Right (env'', LMLValueNil)

lmlEval env (LMLAstLambda captures ids expr) = Right (env, LMLValueFunc (LMLFunction rettype argtypes f))
    where
        capturedVals = sequence $ map (flip lmlEnvLookupData env) captures
        rettype  = LMLTrivialType ""
        argtypes = replicate (length ids) (LMLTrivialType "")

        f :: LMLEnv -> [LMLValue] -> Either String (LMLEnv, LMLValue)
        f localEnv params = do
            capVals <- capturedVals
            let localEnv' = lmlEnvInsertAll (zip captures capVals) localEnv
            (_, res) <- lmlEval (lmlEnvInsertAll (zip ids params) localEnv') expr
            Right (localEnv, res)

lmlEval env (LMLAstFunction name rettype args body) = Right (lmlEnvInsertData name (LMLValueFunc (LMLFunction rettype argtypes f)) env, LMLValueNil)
    where
        expectedRettype = lmlDetermineType env rettype
        argtypes        = map (lmlDetermineType env . snd) args
        argnames        = map fst args

        f :: LMLEnv -> [LMLValue] -> Either String (LMLEnv, LMLValue)
        f localEnv params = do
            (_, res) <- lmlEval (lmlEnvInsertAll (zip argnames params) localEnv) body
            Right (localEnv, res)


lmlEval env (LMLAstIdent i) = ((,) env) <$> lmlEnvLookupData i env
lmlEval env (LMLAstValue v) = Right (env, v)
