module LMLExpr where

import Data.List
import Text.Printf
import Util
import qualified Data.Map.Strict as Map

data LMLType
    = LMLTrivialType String
    | LMLGenericType String
    | LMLListType LMLType
    | LMLFunctionType {
        getReturnType :: LMLType,
        getParamTypes :: [LMLType] }
    | LMLCompoundType {
        getTypename :: String,
        getCTorTypes :: [[LMLType]] }
    | LMLSelfType String -- for use in recursive compound types
    deriving (Show, Eq, Ord)

data LMLList     = LMLList LMLType [LMLValue]
    deriving (Show, Eq)

data LMLCompound = LMLCompound LMLType String [LMLValue]
    deriving (Show, Eq)

data LMLFunction = LMLFunction LMLType [LMLType] (LMLEnv -> [LMLValue] -> Either String (LMLEnv, LMLValue))

instance Eq LMLFunction where
    (==) _ _ = error "Cannot compare functions for equality"

instance Show LMLFunction where
    show (LMLFunction t argTs _) = "LMLFunction " ++ show t ++ " " ++ show argTs ++ " _" 



data LMLValue
    = LMLValueNil
    | LMLValueNum Double
    | LMLValueChar Char
    | LMLValueBool Bool
    | LMLValueList LMLList
    | LMLValueCompound LMLCompound
    | LMLValueFunc LMLFunction
    deriving (Eq, Show)


lmlGetType :: LMLValue -> LMLType
lmlGetType LMLValueNil                                 = LMLTrivialType "()"
lmlGetType (LMLValueNum _)                             = LMLTrivialType "Num"
lmlGetType (LMLValueChar _)                            = LMLTrivialType "Char"
lmlGetType (LMLValueBool _)                            = LMLTrivialType "Bool"
lmlGetType (LMLValueList (LMLList elemType _))         = LMLListType elemType
lmlGetType (LMLValueCompound (LMLCompound t active _)) = t
lmlGetType (LMLValueFunc (LMLFunction ret args _))     = LMLFunctionType ret args


data LMLAst
    = LMLAstValue LMLValue
    | LMLAstList [LMLAst]
    | LMLAstSeq [LMLAst]
    | LMLAstInfix 
        { getLeftOperand :: Maybe LMLAst,
          getRightOperand :: Maybe LMLAst,
          getOperator :: String }
    | LMLAstBlock [LMLAst]
    | LMLAstIdent String
    | LMLAstLetExpr  
        { getLetBindingName :: String, 
          getLetBindingVal :: LMLAst }
    | LMLAstLambda 
        { getLambdaCaptures :: [String], 
          getLambdaParams :: [String], 
          getLambdaBody :: LMLAst }
    | LMLAstFunction 
        { getFuncName :: String,
          getFuncType :: LMLType,
          getFuncParams :: [(String, LMLType)],
          getFuncBody :: LMLAst }
    | LMLAstDataDecl 
        { getDataName :: String,
          getDataCTors :: [(String, [LMLType])] }
    | LMLAstCaseExpr 
        { getCaseInspected :: LMLAst,
          getCaseArms :: [(LMLPat, LMLAst)] }
    | LMLAstIfExpr
        { getConditionAst :: LMLAst,
          getTrueAst :: LMLAst,
          getFalseAst :: LMLAst }
    deriving Show


data LMLPatList'
    = LMLPatListFirstRest LMLPat LMLPat
    | LMLPatListExplicit [LMLPat]
    | LMLPatListNil
    deriving Show


data LMLPat
    = LMLPatIgnore
    | LMLPatList LMLPatList'
    | LMLPatData [LMLPat]
    | LMLPatValue LMLAst
    deriving Show


data LMLEnv = LMLEnv { getData :: Map.Map String LMLValue, getTypes :: Map.Map String LMLType }
    deriving (Show, Eq)


lmlEnvEmpty :: LMLEnv
lmlEnvEmpty = LMLEnv (Map.fromList []) (Map.fromList [])


lmlEnvDefault :: LMLEnv
lmlEnvDefault = LMLEnv (Map.fromList []) (Map.fromList [])


lmlEnvInsertData :: String -> LMLValue -> LMLEnv -> LMLEnv
lmlEnvInsertData ident val (LMLEnv dat types) = LMLEnv (Map.insert ident val dat) types


lmlEnvInsertType :: String -> LMLType -> LMLEnv -> LMLEnv
lmlEnvInsertType ident t (LMLEnv dat types) = LMLEnv dat (Map.insert ident t types)


lmlEnvInsertAll :: [(String, LMLValue)] -> LMLEnv -> LMLEnv
lmlEnvInsertAll ((ident, val):xs) env = lmlEnvInsertAll xs $ lmlEnvInsertData ident val env
lmlEnvInsertAll [] env = env


lmlEnvInsertAllTypes :: [(String, LMLType)] -> LMLEnv -> LMLEnv
lmlEnvInsertAllTypes ((ident, t):xs) env = lmlEnvInsertAllTypes xs $ lmlEnvInsertType ident t env
lmlEnvInsertAllTypes [] env = env


lmlEnvLookupData :: String -> LMLEnv -> Either String LMLValue
lmlEnvLookupData k (LMLEnv dat _) = maybeToEither (printf "did not find value '%s' in env" k) $ Map.lookup k dat


lmlEnvLookupType :: String -> LMLEnv -> Either String LMLType
lmlEnvLookupType s (LMLEnv _ ts) = maybeToEither (printf "did not find type '%s' in env" s) $ Map.lookup s ts


lmlHead :: LMLList -> LMLValue
lmlHead (LMLList t xs) = head xs 


lmlTail :: LMLList -> LMLValue
lmlTail (LMLList t xs) = LMLValueList $ LMLList t (tail xs)
