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


preludeTypes = []

preludeData = [
    ("+",     wrapWithType "(Num -> Num -> Num)" plus),
    ("/",     wrapWithType "(Num -> Num -> Num)" divide),
    ("==",    wrapWithType "(* -> * -> Bool)" equal)]



plus :: LMLEnv -> [LMLValue] -> Either String (LMLEnv, LMLValue)
plus env [LMLValueNum x, LMLValueNum y] = Right (env, LMLValueNum (x + y))
plus _   _                              = Left "invalid call: plus :: Num -> Num -> Num"

divide :: LMLEnv -> [LMLValue] -> Either String (LMLEnv, LMLValue)
divide env [LMLValueNum x, LMLValueNum y] = Right (env, LMLValueNum (x / y))
divide _   _                              = Left "invalid call: divide :: Num -> Num -> Num"

equal :: LMLFunc
equal env [l, r]
    | l == r    = Right (env, LMLValueBool True)
    | otherwise = Right (env, LMLValueBool False)

