module LMLType where

import qualified Data.Map.Strict as Map
import Text.Printf

import LMLExpr




lmlAssignFixedType :: Map.Map String LMLType -> LMLType -> LMLValue -> LMLValue
lmlAssignFixedType genericIdx (LMLFunctionType rettype argtypes) (LMLValueFunc (LMLFunction _ _ f)) = LMLValueFunc (LMLFunction rettype argtypes f)
lmlAssignFixedType genericIdx _ v = v



lmlTypecheck :: Map.Map String LMLType -> LMLType -> LMLValue -> (Map.Map String LMLType, Bool)
lmlTypecheck genericIdx t v = lmlTypecheck' genericIdx t (lmlGetType v)
    where
        lmlTypecheck' :: Map.Map String LMLType -> LMLType -> LMLType -> (Map.Map String LMLType, Bool)

        lmlTypecheck' gIndex (LMLGenericType gt) (LMLTrivialType "") = (gIndex, True)
        lmlTypecheck' gIndex (LMLGenericType gt) t 
            | gt `Map.member` gIndex = (gIndex,                 (gIndex Map.! gt) == t)
            | otherwise              = (Map.insert gt t gIndex, True)

        lmlTypecheck' gIndex (LMLFunctionType lrettype largtypes) (LMLFunctionType rrettype rargtypes)
            = (gIndex'', retMatch && argMatch)

            where
                typefold (gIdx, lastValid) (l, r) = let (gIdx', thisValid) = lmlTypecheck' gIdx l r
                    in (gIdx', thisValid && lastValid) 

                (gIndex',  retMatch) = lmlTypecheck' gIndex lrettype rrettype
                (gIndex'', argMatch) = foldl typefold (gIndex', True) (zip largtypes rargtypes)


        lmlTypecheck' gIndex (LMLTrivialType "") _ = (gIndex, True)
        lmlTypecheck' gIndex (LMLTrivialType l) (LMLTrivialType r)
            | l == r = (gIndex, True)

        lmlTypecheck' gIndex (LMLSelfType self) (LMLCompoundType tname _)
            | self == tname = (gIndex, True)

        lmlTypecheck' gIndex et vt 
            | et == vt  = (gIndex, True)
            | otherwise = error (printf "typecheck failed: expected type: %s got: %s instead" (show et) (show vt))



sequencedTypecheck :: Map.Map String LMLType -> [(LMLType, LMLValue)] -> (Map.Map String LMLType, Bool)
sequencedTypecheck genericIndex tv = sequencedTypecheck' True genericIndex tv
    where
        sequencedTypecheck' valid gIdx []         = (gIdx, valid)
        sequencedTypecheck' valid gIdx ((l,r):xs) = let (gIdx', valid') = lmlTypecheck gIdx l r
            in sequencedTypecheck' (valid && valid') gIdx' xs



lmlResolveGenerics :: Map.Map String LMLType -> LMLType -> LMLType
lmlResolveGenerics gIdx t@(LMLTrivialType _) = t
lmlResolveGenerics gIdx t@(LMLSelfType _)    = t

lmlResolveGenerics gIdx gt@(LMLGenericType t)
    | t `Map.member` gIdx = gIdx Map.! t
    | otherwise           = gt

lmlResolveGenerics gIdx (LMLListType t)                    = LMLListType (lmlResolveGenerics gIdx t)
lmlResolveGenerics gIdx (LMLFunctionType rettype argtypes) = LMLFunctionType (lmlResolveGenerics gIdx rettype) (map (lmlResolveGenerics gIdx) argtypes)
lmlResolveGenerics gIdx self@(LMLCompoundType tname alts)  = LMLCompoundType tname (map (map (lmlResolveGenerics gIdx)) alts)
    where
        collapseSelfType (LMLSelfType t)
            | t == tname = self
        collapseSelfType t = t



lmlDetermineType :: LMLEnv -> LMLType -> LMLType
lmlDetermineType env t@(LMLGenericType gt)      = t
lmlDetermineType env t@(LMLSelfType self)       = t
lmlDetermineType env t@(LMLTrivialType tname)   = either (const t) id (lmlEnvLookupType tname env)
lmlDetermineType env (LMLListType tname)        = LMLListType (lmlDetermineType env tname)
lmlDetermineType env (LMLFunctionType ret args) = LMLFunctionType (lmlDetermineType env ret) (map (lmlDetermineType env) args)
lmlDetermineType env t@(LMLCompoundType _ _)    = t -- DONT KNOW IF THIS IS CORRECT
