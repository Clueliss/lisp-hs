module LMLEval where

import Control.Monad (guard)
import qualified Data.Map.Strict as Map
import Text.Printf
import Data.List
import Data.Either
import Data.Maybe
import System.IO.Unsafe


import LMLExpr
import LMLParser
import LMLType
import Parser
import Util


lmlGenCompoundType :: String -> [[LMLType]] -> LMLEnv -> [[LMLType]]
lmlGenCompoundType tname as env = go as
    where
        go []     = []
        go (x:xs) = (map detType x) : go xs

        detType (LMLTrivialType name)
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
        f (Left e) _                = Left e
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

lmlEval env (LMLAstInfix (Just left) (Just right) op) = lmlEval env (LMLAstSeq [LMLAstIdent op, left, right])

lmlEval env (LMLAstInfix (Just left) Nothing op) = case lmlEval env (LMLAstIdent op) of 
    Right (_, LMLValueFunc func@(LMLFunction rettype argtypes f)) -> do
        guardWithErr "expected binary function in infix expression" (length argtypes == 2)
        (env', left') <- lmlEval env left

        let (_, argOk) = lmlTypecheck (Map.fromList []) (lmlDetermineType env (head argtypes)) left'

        guardWithErr "left operand of binary expression has incorrect type" argOk

        Right (env', LMLValueFunc $ lmlCurry func [left'])

lmlEval env (LMLAstInfix Nothing (Just right) op) = case lmlEval env (LMLAstIdent op) of
    Right (_, LMLValueFunc func@(LMLFunction rettype argtypes f)) -> do
        guardWithErr "expected binary function in infix expression" (length argtypes == 2)
        (env', right') <- lmlEval env right

        let (_, argOk) = lmlTypecheck (Map.fromList []) (lmlDetermineType env (argtypes !! 1)) right'

        guardWithErr "left operand of binary expression has incorrect type" argOk

        let f' e [l] = f e [l, right']

        Right (env', LMLValueFunc $ LMLFunction rettype (tail argtypes) f')


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
    let ctorTypes = lmlGenCompoundType tname (map snd ctors) env

    let newt      = LMLCompoundType tname ctorTypes
    
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

lmlEval env (LMLAstCaseExpr inspected arms) = do
    inspected'             <- snd <$> lmlEval env inspected
    
    patternsMatched        <- maybeToEither "eval error in pattern" $ sequence $ map (lmlCheckPat env inspected') (map fst arms)
    firstPatternMatchedIdx <- maybeToEither "no pattern matched" $ findIndex (== True) patternsMatched

    let matched = arms !! firstPatternMatchedIdx
    env' <- maybeToEither "incompatible pattern type" $ lmlPatModifyEnv inspected' (fst matched) env

    lmlEval env' (snd matched)

lmlEval env (LMLAstIfExpr cond ifTrue ifFalse) = do
    (env', cond') <- lmlEval env cond

    case cond' of 
        LMLValueBool True  -> lmlEval env' ifTrue
        LMLValueBool False -> lmlEval env' ifFalse
        _                                                                           -> Left (printf "condition in if (%s) is not Bool" (show cond'))

lmlEval env (LMLAstIdent i) = ((,) env) <$> lmlEnvLookupData i env
lmlEval env (LMLAstValue v) = Right (env, v)



lmlCheckPat :: LMLEnv -> LMLValue -> LMLPat -> Maybe Bool
lmlCheckPat env x                                 (LMLPatValue (LMLAstIdent id))          = case (lmlEnvLookupData id env) of
    Right val -> Just (x == val)
    Left _    -> Just True

lmlCheckPat env x                                 (LMLPatValue ast)                       = ((== x) . snd) <$> (eitherToMaybe $ lmlEval env ast)
lmlCheckPat _   _                                 LMLPatIgnore                            = Just True
lmlCheckPat _   (LMLValueList (LMLList _ []))     (LMLPatList LMLPatListNil)              = Just True
lmlCheckPat env (LMLValueList (LMLList t (x:xs))) (LMLPatList (LMLPatListFirstRest p ps)) = do
    b  <- lmlCheckPat env x p
    bs <- lmlCheckPat env (LMLValueList (LMLList t xs)) ps

    Just (b && bs)

lmlCheckPat env (LMLValueList (LMLList t xs))     (LMLPatList (LMLPatListExplicit ps))    = and <$> (sequence $ map (uncurry (lmlCheckPat env)) (zip xs ps))
lmlCheckPat _   _                                 _                                       = Just False



lmlPatModifyEnv :: LMLValue -> LMLPat -> LMLEnv -> Maybe LMLEnv
lmlPatModifyEnv val                           (LMLPatValue (LMLAstIdent id))          env = case (lmlEnvLookupData id env) of
    Right val -> Just env
    Left _    -> Just $ lmlEnvInsertData id val env

lmlPatModifyEnv val                           (LMLPatValue _)                         env = Just env
lmlPatModifyEnv val                           (LMLPatIgnore)                          env = Just env
lmlPatModifyEnv val                           (LMLPatList LMLPatListNil)              env = Just env
lmlPatModifyEnv (LMLValueList xs)             (LMLPatList (LMLPatListFirstRest p ps)) env = lmlPatModifyEnv (lmlHead xs) p env >>= lmlPatModifyEnv (lmlTail xs) ps
lmlPatModifyEnv (LMLValueList (LMLList _ xs)) (LMLPatList (LMLPatListExplicit ps))    env = foldl (\e (p,x) -> e >>= lmlPatModifyEnv x p) (Just env) (zip ps xs)
lmlPatModifyEnv _                             _                                       env = Nothing
