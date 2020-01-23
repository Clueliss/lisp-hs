module LMLParser (module LMLParser, module Parser) where

import Control.Applicative
import Data.Char
import qualified Data.Set as Set
import Text.Read

import Parser
import LMLExpr

asciiPunct :: Set.Set Char
asciiPunct = Set.fromAscList "!$%&*+-/:<>?@^|"


lmlDeclParser :: Parser (String, LMLType)
lmlDeclParser = (,)
        <$> identParser
        <*> ((wsP *> charP ':' *> wsP *> lmlTypeParser) <|> pure (LMLTrivialType ""))


lmlTypeParser :: Parser LMLType
lmlTypeParser = (LMLTrivialType <$> identParser) 
    <|> (LMLListType <$> listTP)
    <|> (uncurry (flip LMLFunctionType) <$> funTP)
    <|> (LMLCompoundType "Tuple" . (: []) <$> tupleTP)
    
    where
        funTP = (,) <$> tupleTP <* (wsP *> strP "->" <* wsP) <*> lmlTypeParser
        
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




ctorDeclParser :: Parser (String, [String])
ctorDeclParser = (,) 
    <$> (identParser <* wsP)
    <*> (sepBy wsP identParser)


lmlDataDeclParser :: Parser LMLAst
lmlDataDeclParser = LMLAstDataDecl
    <$> (strP "data" *> wsP *> identParser <* wsP <* charP '=' <* wsP)
    <*> sepBy (wsP *> charP '|' <* wsP) ctorDeclParser
        


{-

lispFunExpr :: Parser LMLAst
lispFunExpr = (\ident params body -> LispFunExpr (ident, params, body))
    <$> (strP "fun" *> wsP *> identParser <* wsP)
    <*> (charP '(' *> wsP *> (map fst <$> sepBy (wsP *> charP ',' <* wsP) declParser) <* wsP <* charP ')')
    <*> (wsP *> charP '=' *> wsP *> lispExprParser)


lispInfixParser :: Parser LMLAst
lispInfixParser = (\a op b -> LispInfixExpr (op, a, b))
    <$> (charP '(' *> wsP *> lispExprParser)
    <*> (wsP *> (some $ parseIf (flip elem asciiPunct)) <* wsP)
    <*> (lispExprParser <* wsP <* charP ')')

-}

lmlParser :: Parser LMLAst
lmlParser =
    lmlLetExprParser <|>
    lmlLambdaParser <|>
    lmlDataDeclParser <|>
    lmlStrParser <|>
    lmlCharParser <|>
    lmlNumParser <|>
    lmlIdentParser <|>
    lmlListParser <|>
    lmlBlockParser <|>
    lmlSeqParser
