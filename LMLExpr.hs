module LMLExpr where

import Data.List
import qualified Data.Map.Strict as Map

data LMLType
    = LMLTrivialType String
    | LMLGenericType String
    | LMLListType LMLType
    | LMLFunctionType LMLType [LMLType]
    | LMLCompoundType String [[LMLType]]
    | LMLSelfType String -- for use in recursive compound types
    deriving (Show, Eq, Ord)

data LMLList     = LMLList LMLType [LMLValue]
    deriving (Show, Eq)

data LMLCompound = LMLCompound LMLType String [LMLValue]
    deriving (Show, Eq)

data LMLFunction = LMLFunction LMLType [LMLType] (LMLEnv -> [LMLValue] -> Maybe (LMLEnv, LMLValue))

instance Eq LMLFunction where
    (==) _ _ = error "Cannot compare functions for equality"

instance Show LMLFunction where
    show (LMLFunction t argTs _) = "LMLFunction " ++ show t ++ " " ++ show argTs ++ " _" 



data LMLValue
    = LMLValueNil
    | LMLValueNum Double
    | LMLValueChar Char
    | LMLValueList LMLList
    | LMLValueCompound LMLCompound
    | LMLValueFunc LMLFunction
    deriving (Eq, Show)


lmlGetType :: LMLValue -> LMLType
lmlGetType LMLValueNil                                 = LMLTrivialType "()"
lmlGetType (LMLValueNum _)                             = LMLTrivialType "Num"
lmlGetType (LMLValueChar _)                            = LMLTrivialType "Char"
lmlGetType (LMLValueList (LMLList elemType _))         = LMLListType elemType
lmlGetType (LMLValueCompound (LMLCompound t active _)) = t
lmlGetType (LMLValueFunc (LMLFunction ret args _))   = LMLFunctionType ret args


data LMLAst
    = LMLAstValue LMLValue
    | LMLAstList [LMLAst]
    | LMLAstSeq [LMLAst]
    | LMLAstBlock [LMLAst]
    | LMLAstIdent String
    | LMLAstLetExpr String LMLAst
    | LMLAstLambda [String] [String] LMLAst
    | LMLAstFunction String LMLType [(String, LMLType)] LMLAst
    | LMLAstDataDecl String [(String, [String])]
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


lmlEnvLookupData :: String -> LMLEnv -> Maybe LMLValue
lmlEnvLookupData k (LMLEnv dat _) = Map.lookup k dat


lmlEnvLookupType :: String -> LMLEnv -> Maybe LMLType
lmlEnvLookupType s (LMLEnv _ ts) = Map.lookup s ts


lmlDetermineType :: LMLEnv -> LMLType -> LMLType
lmlDetermineType env t@(LMLGenericType gt)      = t
lmlDetermineType env t@(LMLTrivialType tname)   = maybe t id (lmlEnvLookupType tname env)
lmlDetermineType env (LMLListType tname)        = (LMLListType (lmlDetermineType env tname))
lmlDetermineType env (LMLFunctionType ret args) = (LMLFunctionType (lmlDetermineType env ret) (map (lmlDetermineType env) args))
