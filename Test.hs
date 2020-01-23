module Test where

import Control.Applicative
import Control.Exception
import Data.List (intercalate)
import Data.Map (toList)
import Data.Maybe (fromJust)
import Text.Printf (printf)

import LMLEval
import LMLExpr
import LMLParser



assertEq :: Eq a => a -> a -> String -> e -> e
assertEq a b msg expr = if a == b
    then expr
    else error msg


ppEnv :: LMLEnv -> String
ppEnv (LMLEnv dat types) = 
    "types:\n " ++ go showTypeDecl types ++ 
    "values:\n" ++ go showValDecl dat

    where
        go f m = unlines $ map (('\t' :) . uncurry f) $ toList m

        showTypeDecl name t  = printf "%s = %s" name (ppExpandType t)
        showValDecl name val = printf "%s = %s :: %s" name (ppVal val) (ppType (lmlGetType val))


ppExpandType :: LMLType -> String
ppExpandType (LMLCompoundType _ ctors) = intercalate " | " $ map (unwords . map ppType) ctors
ppExpandType t = ppType t


ppType :: LMLType -> String
ppType (LMLTrivialType t)                 = t
ppType (LMLListType t)                    = printf "[%s]" $ ppType t
ppType (LMLFunctionType rettype argTypes) = printf "%s -> %s" (intercalate " -> " $ map ppType argTypes) (ppType rettype)
ppType (LMLCompoundType name _)           = name
ppType (LMLSelfType name)                 = name


ppVal :: LMLValue -> String
ppVal = ppVal' 0
    where
        ppVal' _     LMLValueNil                                    = "()"
        ppVal' _     (LMLValueNum x)                                = show x
        ppVal' _     (LMLValueChar ch)                              = show ch
        ppVal' _     (LMLValueCompound (LMLCompound _ active []))   = active
        ppVal' depth (LMLValueList (LMLList _ xs))                  = "[" ++ intercalate ", " (map (ppVal' depth) xs) ++ "]"
        ppVal' depth (LMLValueCompound (LMLCompound _ active vals)) = if depth > 0
            then printf "(%s)" ppVa
            else ppVa
            where
                ppVa = printf "%s %s" active (unwords $ map (ppVal' $ depth + 1) vals)

        ppVal' _ other = "?"


ppValWithType :: LMLValue -> String
ppValWithType v = printf "%s :: %s" (ppVal v) (ppType $ lmlGetType v)




doEval :: LMLEnv -> String -> Maybe (LMLEnv, LMLValue)
doEval env input = do
    (expr, rest) <- runParser lmlParser input
    lmlEval env expr



env = lmlEnvEmpty

testProgram' = "{ data List = Cons Num List | Empty; [ (Cons 0 (Cons 1 (Cons 1.2 Empty))), (Cons 0 Empty) ] }" -- todo this should not typecheck

testProgram = "{ data List = Cons Num List | Empty; data Bool = True | False; False }"


test = let (env', res) = fromJust $ doEval env testProgram
    in putStrLn (ppEnv env') >> putStrLn ("eval result: " ++ ppValWithType res)
