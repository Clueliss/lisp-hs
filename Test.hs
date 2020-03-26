module Test where

import Control.Applicative
import Control.Exception
import Data.Either
import Data.List (intercalate)
import Data.Map (toList)
import Data.Maybe (fromJust)
import Text.Printf (printf)
import Control.Monad.State

import LMLEval
import LMLExpr
import LMLParser
import LMLPrelude
import Util


ppEnv :: LMLEnv -> String
ppEnv (LMLEnv dat types) = 
    "types:\n" ++ go showTypeDecl types ++ 
    "values:\n" ++ go showValDecl dat

    where
        go f m = unlines $ map (('\t' :) . uncurry f) $ toList m

        showTypeDecl name t  = printf "%s = %s" name (ppExpandType t)
        showValDecl name val = printf "%s = %s :: %s" name (ppVal val) (ppType (lmlGetType val))


ppExpandType :: LMLType -> String
ppExpandType (LMLCompoundType _ ctors) = intercalate " | " $ map (replaceOnEmpty "()" . unwords . map ppType) ctors
    where
        replaceOnEmpty x [] = x
        replaceOnEmpty _ xs = xs 

ppExpandType t = ppType t


ppType :: LMLType -> String
ppType (LMLTrivialType "")                = "*"
ppType (LMLGenericType gt)                = "'" ++ gt
ppType (LMLTrivialType t)                 = t
ppType (LMLListType t)                    = printf "[%s]" $ ppType t
ppType (LMLFunctionType rettype argTypes) = printf "(%s -> %s)" (intercalate " -> " $ map ppType argTypes) (ppType rettype)
ppType (LMLCompoundType name _)           = name
ppType (LMLSelfType name)                 = name


ppVal :: LMLValue -> String
ppVal = ppVal' 0
    where
        ppVal' _     LMLValueNil                                    = "()"
        ppVal' _     (LMLValueNum x)                                = show x
        ppVal' _     (LMLValueChar ch)                              = show ch
        ppVal' _     (LMLValueBool b)                               = show b
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




doEval :: LMLEnv -> String -> Either String (LMLEnv, LMLValue)
doEval env input = do
    (expr, rest) <- maybeToEither "invalid lml" $ runParser lmlParser input
    lmlEval env expr


preludeEnv :: LMLEnv
preludeEnv = lmlEnvInsertAllTypes preludeTypes $ lmlEnvInsertAll preludeData $ lmlEnvEmpty


envSetup :: [String]
envSetup = [
    "data List = Cons * List | Nil",
    "fun f x y :: [List] -> Num -> Num = 4",
    "fun h g x :: ('a -> 'a -> 'b) -> 'a -> 'b = (g x x)",
    "fun g x :: Num -> Num = (x + 1)",
    "fun const x _ :: 'a -> * -> 'a = x",
    "fun compose f g x :: ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c = (g (f x))"]


env :: LMLEnv
env = fst $ unwrapRight $ lmlSequencedEval preludeEnv $ map (fst . fromJust . runParser lmlParser) $ envSetup


getLML :: String -> LMLValue
getLML input = snd $ unwrapRight $ doEval env input


runLML :: String -> IO ()
runLML input = let (env', res) = unwrapRight $ doEval env input
    in putStrLn (ppEnv env') >> putStrLn ("eval result: " ++ ppValWithType res)


lmlInteractive :: LMLEnv -> IO ()
lmlInteractive e = do
    putStr "lml> "
    input <- getLine

    case input of 
        (':':'q':[])       -> pure ()
        (':':'r':[])       -> lmlInteractive env -- TODO properly reset
        
        (':':'t':' ':code) -> case doEval e code of
            Right (e', val) -> putStrLn (ppType $ lmlGetType val) >> lmlInteractive e
            Left err        -> putStrLn ("Error: " ++ err)        >> lmlInteractive e
        
        (':':'v':' ':code) -> case doEval e code of
            Right (e', val) -> putStrLn (ppValWithType val) >> lmlInteractive e'
            Left err        -> putStrLn ("Error: " ++ err)  >> lmlInteractive e

        (':':_)            -> putStrLn "Unknown command" >> lmlInteractive e

        code -> case doEval e code of
            Right (e', val) -> putStrLn (ppVal val)        >> lmlInteractive e'
            Left err        -> putStrLn ("Error: " ++ err) >> lmlInteractive e
