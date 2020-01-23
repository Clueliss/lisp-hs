module LMLExpr where

import Data.List
import qualified Data.Map.Strict as Map

data LMLType
    = LMLTrivialType String
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
lmlGetType (LMLValueFunc (LMLFunction ret args _))     = LMLFunctionType ret args


data LMLAst
    = LMLAstValue LMLValue
    | LMLAstList [LMLAst]
    | LMLAstSeq [LMLAst]
    | LMLAstBlock [LMLAst]
    | LMLAstIdent String
    | LMLAstLetExpr String LMLAst
    | LMLAstLambda [String] LMLAst
    | LMLAstDataDecl String [(String, [String])]
    deriving Show



data LMLEnv = LMLEnv { getData :: Map.Map String LMLValue, getTypes :: Map.Map String LMLType }
    deriving (Show, Eq)


lmlEnvEmpty :: LMLEnv
lmlEnvEmpty = LMLEnv (Map.fromList []) (Map.fromList [])


lmlBuiltinTypes :: Map.Map String LMLType
lmlBuiltinTypes = Map.fromList [
    ("Num", LMLTrivialType "Num"),
    ("Char", LMLTrivialType "Char")]


lmlEnvDefault :: LMLEnv
lmlEnvDefault = LMLEnv (Map.fromList []) (Map.fromList [])


lmlEnvInsertData :: String -> LMLValue -> LMLEnv -> LMLEnv
lmlEnvInsertData ident val (LMLEnv dat types) = LMLEnv (Map.insert ident val dat) types


lmlEnvInsertType :: String -> LMLType -> LMLEnv -> LMLEnv
lmlEnvInsertType ident t (LMLEnv dat types) = LMLEnv dat (Map.insert ident t types)


lmlEnvInsertAll :: [(String, LMLValue)] -> LMLEnv -> LMLEnv
lmlEnvInsertAll ((ident, val):xs) (LMLEnv dat types) = lmlEnvInsertAll xs (LMLEnv (Map.insert ident val dat) types)
lmlEnvInsertAll [] env = env


lmlEnvlookupData :: String -> LMLEnv -> Maybe LMLValue
lmlEnvlookupData k (LMLEnv dat _) = Map.lookup k dat


lmlEnvLookupType :: String -> LMLEnv -> Maybe LMLType
lmlEnvLookupType s (LMLEnv _ ts) = Map.lookup s ts


lmlDetermineType :: LMLType -> LMLEnv -> LMLType
lmlDetermineType t@(LMLTrivialType tname) env = maybe t id (lmlEnvLookupType tname env)
lmlDetermineType t _ = t
