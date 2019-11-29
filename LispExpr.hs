module LispExpr where

import Data.Map (Map, fromList, insert)

import MathExpr


data LispExpr
    = LispNil
    | LispBool Bool
    | LispNum Double
    | LispList [LispExpr]
    | LispSeq [LispExpr]
    | LispBlock [LispExpr]
    | LispStr String
    | LispIdent String
    | LispFun (LispEnv -> [LispExpr] -> Maybe LispExpr)
    | LispIntrinsicFun (LispEnv -> [LispExpr] -> Maybe (LispEnv, LispExpr))
    | LispLetExpr (String, LispExpr, LispExpr)
    | LispFunExpr (String, [String], LispExpr)
    | LispLambdaExpr ([String], LispExpr)
    | LispInfixExpr (Char, LispExpr, LispExpr)

instance Show LispExpr where
    show LispNil = "LispNil"
    show (LispBool b) = "LispBool " ++ show b
    show (LispNum n) = "LispNum " ++ show n
    show (LispStr s) = "LispStr " ++ show s
    show (LispIdent i) = "LispIdent " ++ i
    show (LispFun _) = "LispFun"
    show (LispIntrinsicFun _) = "LispIntrinsicFun"
    show (LispList l) = "LispList " ++ show l
    show (LispSeq s) = "LispSeq " ++ show s
    show (LispBlock blk) = "LispBlock " ++ show blk
    show (LispLetExpr (id, val, expr)) = "LispLetExpr let " ++ id ++ " = " ++ show val ++ " in " ++ show expr


    
type LispEnv = Map String LispExpr


emptyLispEnv :: LispEnv
emptyLispEnv = (fromList [])


lispEnvInsertAll :: [(String, LispExpr)] -> LispEnv -> LispEnv
lispEnvInsertAll ((id, val):xs) env = lispEnvInsertAll xs (insert id val env)
lispEnvInsertAll [] env = env
