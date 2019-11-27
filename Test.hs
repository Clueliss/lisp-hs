module Test where

import Parser
import LispValue
import Evaluator

import Data.Maybe (fromJust)
import Data.Map (insert)

intoIdents :: [LispValue] -> Maybe [String]
intoIdents v = sequenceA $ map f v
    where
        f (LispIdent i) = Just i
        f _ = Nothing


fun :: LispEnv -> [LispValue] -> Maybe LispValue
fun env [LispList paramidents, bodyexpr] = Just $ LispIntrinsicFun $ \env params -> do
        ids <- intoIdents paramidents
        vals <- sequenceA $ map (lispEval env) params

        lispEval (lispEnvInsertAll (zip ids vals) env) bodyexpr

fun env _ = Nothing



env = (insert "fun" (LispIntrinsicFun fun) emptyLispEnv)

test = lispEval env (fst $ fromJust (parse lispValueParser "((fun (a b) a a) 5 123)"))
