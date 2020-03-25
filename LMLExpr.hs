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

data LMLFunction = LMLFunction LMLType [LMLType] (LMLEnv -> [LMLValue] -> Either String (LMLEnv, LMLValue))

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
          getDataCTors :: [(String, [String])] }
    | LMLAstCaseExpr 
        { getCaseInspected :: LMLAst,
          getCaseArms :: [(LMLPat, LMLAst)] } 
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


fromJustOrErr :: e -> Maybe a -> Either e a
fromJustOrErr e (Just x) = Right x
fromJustOrErr e Nothing  = Left e


lmlEnvLookupData :: String -> LMLEnv -> Either String LMLValue
lmlEnvLookupData k (LMLEnv dat _) = fromJustOrErr "" $ Map.lookup k dat


lmlEnvLookupType :: String -> LMLEnv -> Either String LMLType
lmlEnvLookupType s (LMLEnv _ ts) = fromJustOrErr "" $ Map.lookup s ts


lmlHead :: LMLList -> LMLValue
lmlHead (LMLList t xs) = head xs 


lmlTail :: LMLList -> LMLValue
lmlTail (LMLList t xs) = LMLValueList $ LMLList t (tail xs)