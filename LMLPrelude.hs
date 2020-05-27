module LMLPrelude where

import Data.Maybe

import Parser
import LMLExpr
import LMLParser



type LMLFunc = LMLEnv -> [LMLValue] -> Either String (LMLEnv, LMLValue)

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
    ("/",     wrapWithType "(Num -> Num -> Num)" divide),
    ("==",    LMLValueFunc $ LMLFunction lmlBoolType [LMLTrivialType "Num", LMLTrivialType "Num"] equal),
    ("True",  lmlBoolTrue),
    ("False", lmlBoolFalse)]



plus :: LMLEnv -> [LMLValue] -> Either String (LMLEnv, LMLValue)
plus env [LMLValueNum x, LMLValueNum y] = Right (env, LMLValueNum (x + y))
plus _   _                              = Left "invalid call: + :: Num -> Num -> Num"


divide :: LMLEnv -> [LMLValue] -> Either String (LMLEnv, LMLValue)
divide env [LMLValueNum x, LMLValueNum y] = Right (env, LMLValueNum (x / y))
divide _   _                              = Left "invalid call to / :: Num -> Num -> Num"



equal :: LMLFunc
equal env [l, r]
    | l == r    = Right (env, lmlBoolTrue)
    | otherwise = Right (env, lmlBoolFalse)

