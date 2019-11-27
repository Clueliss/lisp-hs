module LispValue where

import Data.Map (Map, fromList, insert)

data LispValue
    = LispNil
    | LispBool Bool
    | LispNum Float
    | LispList [LispValue]
    | LispStr String
    | LispIdent String
    | LispFun (LispEnv -> [LispValue] -> LispValue)
    | LispIntrinsicFun (LispEnv -> [LispValue] -> Maybe LispValue)


instance Show LispValue where
    show LispNil = "()"
    show (LispBool b) = show b
    show (LispNum n) = show n
    show (LispStr s) = show s
    show (LispIdent i) = "ident " ++ i
    show (LispFun _) = "LispFun"
    show (LispIntrinsicFun _) = "LispIntrinsicFun"
    show (LispList l) = show l


    
type LispEnv = Map String LispValue


emptyLispEnv :: LispEnv
emptyLispEnv = (fromList [])


lispEnvInsertAll :: [(String, LispValue)] -> LispEnv -> LispEnv
lispEnvInsertAll ((id, val):xs) env = lispEnvInsertAll xs (insert id val env)
lispEnvInsertAll [] env = env
