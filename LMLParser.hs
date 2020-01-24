module LMLParser (module LMLParser, module Parser) where

import Control.Applicative
import Data.Char
import qualified Data.Set as Set
import Data.Tuple
import Text.Read

import Parser
import LMLExpr

asciiPunct :: Set.Set Char
asciiPunct = Set.fromAscList "!$%&*+-/:<>?@^|="

symbolParser :: Parser String
symbolParser = some $ parseIf $ flip elem asciiPunct


-- x: Int -> y: Int -> Int
lmlFuncDeclParser :: Parser (LMLType, [(String, LMLType)])
lmlFuncDeclParser = (\x y -> (y, x))
    <$> (sepBy (wsP *> strP "->" <* wsP) lmlDeclParser)
    <*> (wsP *> strP "->" *> wsP *> parseOpt lmlTypeParser (LMLTrivialType ""))


lmlDeclParser :: Parser (String, LMLType)
lmlDeclParser = (,)
        <$> identParser
        <*> (wsP *> charP ':' *> wsP *> parseOpt lmlTypeParser (LMLTrivialType ""))


lmlTypeParser :: Parser LMLType
lmlTypeParser = funTP
    <|> (LMLTrivialType <$> identParser) 
    <|> (LMLListType <$> listTP)
    <|> (LMLCompoundType "Tuple" . (: []) <$> tupleTP)
    
    where
        optTP = parseOpt lmlTypeParser (LMLTrivialType "")

        funTP = (\x xs -> LMLFunctionType (last xs) (x:(init xs)))
            <$> (charP '(' *> wsP *> optTP)
            <*> (some (wsP *> strP "->" *> wsP *> optTP)) <* wsP <* charP ')'
        
        listTP  = (charP '[' *> wsP *> 
            lmlTypeParser 
            <* wsP <* charP ']')

        tupleTP = (charP '(' *> wsP *> 
            (sepBy (wsP *> charP ',' <* wsP) lmlTypeParser)
            <* wsP <* charP ')')



lmlNumParser :: Parser LMLAst
lmlNumParser = LMLAstValue . LMLValueNum <$> floatParser


lmlCharParser :: Parser LMLAst
lmlCharParser = LMLAstValue . LMLValueChar <$> (charP '\'' *> charParser <* charP '\'')


lmlStrParser :: Parser LMLAst
lmlStrParser = LMLAstValue . LMLValueList . (LMLList (LMLTrivialType "Char")) . map LMLValueChar <$> (charP '"' *> many normalCharParser <* charP '"')


lmlIdentParser :: Parser LMLAst
lmlIdentParser = LMLAstIdent <$> identParser

lmlSymbolParser :: Parser LMLAst
lmlSymbolParser = LMLAstIdent <$> symbolParser


lmlListParser :: Parser LMLAst
lmlListParser = LMLAstList <$> (charP '[' *> wsP *> (sepBy sepP lmlParser) <* wsP <* charP ']')
    where
        sepP = wsP *> parseOpt (charP ',') ' ' <* wsP


lmlSeqParser :: Parser LMLAst
lmlSeqParser = LMLAstSeq <$> (charP '(' *> wsP *> (sepBy wsP lmlParser) <* wsP <* charP ')')


lmlBlockParser :: Parser LMLAst
lmlBlockParser = LMLAstBlock <$> (charP '{' *> wsP *> (sepBy sepP lmlParser) <* wsP <* charP '}')
    where
        sepP = wsP *> parseOpt (charP ';') ' ' <* wsP


lmlLetExprParser :: Parser LMLAst
lmlLetExprParser = LMLAstLetExpr
    <$> (strP "let" *> wsP *> identParser <* wsP <* charP '=' <* wsP) 
    <*> lmlParser


lmlLambdaParser :: Parser LMLAst
lmlLambdaParser = LMLAstLambda
    <$> (strP "fn" *> wsP *> charP '(' *> wsP *> (sepBy wsP identParser) <* wsP <* charP ')')
    <*> (wsP *> strP "->" *> wsP *> lmlParser)


lmlFunctionParser :: Parser LMLAst
lmlFunctionParser = (\funname (ret, args) body -> LMLAstFunction funname ret args body) 
    <$> (strP "fun" *> wsP *> (identParser <|> symbolParser) <* wsP)
    <*> (charP '(' *> wsP *> lmlFuncDeclParser <* wsP <* charP ')' <* wsP)
    <*> (charP '=' *> wsP *> lmlParser)


ctorDeclParser :: Parser (String, [String])
ctorDeclParser = (,) 
    <$> (identParser <* wsP)
    <*> (sepBy wsP identParser)


lmlDataDeclParser :: Parser LMLAst
lmlDataDeclParser = LMLAstDataDecl
    <$> (strP "data" *> wsP *> identParser <* wsP <* charP '=' <* wsP)
    <*> sepBy (wsP *> charP '|' <* wsP) ctorDeclParser
        


lmlInfixParser :: Parser LMLAst
lmlInfixParser = (\a op b -> LMLAstSeq [LMLAstIdent op, a, b])
    <$> (charP '(' *> wsP *> lmlParser)
    <*> (wsP *> symbolParser <* wsP)
    <*> (lmlParser <* wsP <* charP ')')


lmlParser :: Parser LMLAst
lmlParser =
    lmlLetExprParser <|>
    lmlLambdaParser <|>
    lmlFunctionParser <|>
    lmlDataDeclParser <|>
    lmlStrParser <|>
    lmlCharParser <|>
    lmlNumParser <|>
    lmlIdentParser <|>
    lmlSymbolParser <|>
    lmlListParser <|>
    lmlBlockParser <|>
    lmlInfixParser <|>
    lmlSeqParser
