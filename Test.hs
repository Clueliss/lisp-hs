module Test where

import Control.Exception
import Data.Maybe (fromJust)
import Data.Map (insert)

import LispParser
import LispExpr
import LispEval



assertEq :: Eq a => a -> a -> String -> e -> e
assertEq a b msg expr = if a == b
    then expr
    else error msg


doEval :: LispEnv -> String -> Maybe (LispEnv, LispExpr)
doEval env input = do
    (expr, rest) <- parse lispExprParser input
    lispEval env expr



env = emptyLispEnv

test = doEval env "{ fun f (x y) = let z = (x + y) in (3 * z); (f 1 2) }"
