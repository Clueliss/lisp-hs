module Evaluator where

import LispValue
import Data.Map as Map (lookup, insert)
import Data.Maybe (fromJust)


lispEval :: LispEnv -> LispValue -> Maybe LispValue
lispEval env (LispList (x:xs)) = f (lispEval env x)
    where
        f :: Maybe LispValue -> Maybe LispValue

        f (Just (LispIntrinsicFun g)) = g env xs
        
        f (Just (LispFun h)) = do
            vals <- sequenceA $ map (lispEval env) xs
            Just (h env vals)

        f (Just y) = LispList <$> sequenceA (Just y : (map (lispEval env) xs))

        f _ = Nothing

lispEval env (LispIdent id) = Map.lookup id env
lispEval env val = Just val
