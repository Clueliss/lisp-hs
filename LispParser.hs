module LispParser (module LispParser, module Parser) where

import Control.Applicative
import Data.Char
import Text.Read

import Parser
import LispExpr
import MathExpr (isMathOp)


lispNumParser :: Parser LispExpr
lispNumParser = LispNum <$> floatParser


lispBoolParser :: Parser LispExpr
lispBoolParser = (LispBool True <$ t) <|> (LispBool False <$ f)
    where
        t = strP "true"
        f = strP "false"


lispStrParser :: Parser LispExpr
lispStrParser = LispStr <$> (charP '"' *> many (normalCharParser <|> escapedCharParser) <* charP '"')


lispIdentParser :: Parser LispExpr
lispIdentParser = LispIdent <$> identParser


lispListParser :: Parser LispExpr
lispListParser = LispList <$> (charP '[' *> wsP *> (sepBy sepP lispExprParser) <* wsP <* charP ']')
    where
        sepP = wsP *> parseOpt (charP ',') ' ' <* wsP


lispSeqParser :: Parser LispExpr
lispSeqParser = LispSeq <$> (charP '(' *> wsP *> (sepBy wsP lispExprParser) <* wsP <* charP ')')


lispBlockParser :: Parser LispExpr
lispBlockParser = LispBlock <$> (charP '{' *> wsP *> (sepBy sepP lispExprParser) <* wsP <* charP '}')
    where
        sepP = wsP *> parseOpt (charP ';') ' ' <* wsP


lispLetExpr :: Parser LispExpr
lispLetExpr = (\ident value expr -> LispLetExpr (ident, value, expr)) 
    <$> (strP "let" *> wsP *> identParser <* wsP <* charP '=' <* wsP) 
    <*> lispExprParser
    <*> (wsP *> strP "in" *> wsP *> lispExprParser)


lispFunExpr :: Parser LispExpr
lispFunExpr = (\ident params body -> LispFunExpr (ident, params, body))
    <$> (strP "fun" *> wsP *> identParser <* wsP)
    <*> (charP '(' *> wsP *> (sepBy wsP identParser) <* wsP <* charP ')')
    <*> (wsP *> charP '=' *> wsP *> lispExprParser)


lispLambdaParser :: Parser LispExpr
lispLambdaParser = (\params body -> LispLambdaExpr (params, body))
    <$> (strP "fn" *> wsP *> charP '(' *> wsP *> (sepBy wsP identParser) <* wsP <* charP ')')
    <*> (wsP *> strP "->" *> wsP *> lispExprParser)


lispInfixParser :: Parser LispExpr
lispInfixParser = (\a op b -> LispInfixExpr (op, a, b))
    <$> (charP '(' *> wsP *> lispExprParser)
    <*> (wsP *> (parseIf isMathOp) <* wsP)
    <*> (lispExprParser <* wsP <* charP ')')


lispExprParser :: Parser LispExpr
lispExprParser =
    lispBoolParser <|>
    lispLetExpr <|>
    lispFunExpr <|>
    lispLambdaParser <|>
    lispStrParser <|>
    lispNumParser <|>
    lispIdentParser <|>
    lispListParser <|>
    lispBlockParser <|>
    lispInfixParser <|>
    lispSeqParser
