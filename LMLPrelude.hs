module LMLPrelude where

import Data.Maybe

import Parser
import LMLExpr
import LMLParser



type LMLFunc = LMLEnv -> [LMLValue] -> Maybe (LMLEnv, LMLValue)

wrapWithType :: String -> LMLFunc -> LMLValue
wrapWithType typestr f = LMLValueFunc $ LMLFunction rettype argtypes f  
    where
        LMLFunctionType rettype argtypes = fst $ fromJust $ runParser lmlTypeParser typestr



lmlBoolType = LMLCompoundType "Bool" [[], []]

lmlBoolTrue  = LMLValueCompound $ LMLCompound lmlBoolType "True" []
lmlBoolFalse = LMLValueCompound $ LMLCompound lmlBoolType "False" []



preludeTypes = [
    ("Bool", lmlBoolType)]

preludeData = [
    ("+",     wrapWithType "(Num -> Num -> Num)" plus),
    ("==",    LMLValueFunc $ LMLFunction lmlBoolType [LMLTrivialType "Num", LMLTrivialType "Num"] equal),
    ("True",  lmlBoolTrue),
    ("False", lmlBoolFalse)]



plus :: LMLEnv -> [LMLValue] -> Maybe (LMLEnv, LMLValue)
plus env [LMLValueNum x, LMLValueNum y] = Just (env, LMLValueNum (x + y))


equal :: LMLFunc
equal env [l, r]
    | l == r    = Just (env, lmlBoolTrue)
    | otherwise = Just (env, lmlBoolFalse) 

