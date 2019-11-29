module MathExpr where

import Control.Applicative
import Data.Maybe
import Data.Set (Set, member, fromList)

import Parser


data MathExpr
    = MathExprNum Double
    | MathExprOp (Char, MathExpr, MathExpr)


instance Show MathExpr where
    show (MathExprNum x) = show x
    show (MathExprOp (op, x, y)) = "(" ++ (show x) ++ " " ++ [op] ++ " " ++ (show y) ++ ")"



mathOps :: Set Char
mathOps = fromList ['*', '+', '-', '/', '%']

isMathOp :: Char -> Bool
isMathOp ch = member ch mathOps 


opParser :: Parser MathExpr
opParser = (\a op b -> MathExprOp (op, a, b))
    <$> (charP '(' *> wsP *> mathExprParser)
    <*> (wsP *> (parseIf isMathOp) <* wsP)
    <*> (mathExprParser <* wsP <* charP ')')


mathExprParser :: Parser MathExpr
mathExprParser = opParser
    <|> (MathExprNum <$> floatParser)


evalMathExpr :: MathExpr -> Maybe Double
evalMathExpr (MathExprNum x) = Just x
evalMathExpr (MathExprOp (op, a, b)) = do
    evala <- evalMathExpr a
    evalb <- evalMathExpr b

    case op of
        '+' -> Just (evala + evalb)
        '*' -> Just (evala * evalb)
        '-' -> Just (evala - evalb)
        '/' -> Just (evala / evalb)
        '%' -> Just $ fromIntegral (mod ((floor evala)::Integer) ((floor evalb)::Integer))
        otherwise -> Nothing
