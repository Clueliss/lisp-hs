module LMLParser (module LMLParser, module Parser) where

import Control.Applicative
import Data.Char
import qualified Data.Set as Set
import Data.Tuple
import Text.Read
import Data.Function

import Parser
import LMLExpr

asciiPunct :: Set.Set Char
asciiPunct = Set.fromAscList "!$%&*+-/:<>?@^|="

symbolParser :: Parser String
symbolParser = some $ parseIf (`elem` asciiPunct)


-- x: Int -> y: Int -> Int
lmlFuncDeclParser :: Parser (LMLType, [(String, LMLType)])
lmlFuncDeclParser = (\x y -> (y, x))
    <$> (sepBy (wsP *> strP "->" <* wsP) lmlDeclParser)
    <*> (wsP *> strP "->" *> wsP *> lmlTypeParser)


lmlDeclParser :: Parser (String, LMLType)
lmlDeclParser = (,)
    <$> identParser
    <*> (wsP *> charP ':' *> wsP *> lmlTypeParser)


lmlTypeParser :: Parser LMLType
lmlTypeParser = (LMLTrivialType "" <$ charP '*')
    <|> genericTP 
    <|> funTP
    <|> (LMLTrivialType <$> identParser) 
    <|> (LMLListType <$> listTP)
    <|> (LMLCompoundType "Tuple" . (: []) <$> tupleTP)
    
    where
        genericTP = LMLGenericType <$> (charP '\'' *> identParser)

        funTP = (\x xs -> LMLFunctionType (last xs) (x:(init xs)))
            <$> (charP '(' *> wsP *> lmlTypeParser)
            <*> (some (wsP *> strP "->" *> wsP *> lmlTypeParser)) <* wsP <* charP ')'
        
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
    <$> (strP "fn" *> wsP *> parseOpt (charP '[' *> sepBy (wsP *> charP ',' <* wsP) identParser <* charP ']') [])
    <*> (wsP *> charP '(' *> wsP *> (sepBy wsP identParser) <* wsP <* charP ')')
    <*> (wsP *> strP "->" *> wsP *> lmlParser)



-- example: fun compose ( f:('b -> 'c) -> g:('a -> 'b) -> x:'a -> 'c ) = (f (g x))
lmlFunctionParser :: Parser LMLAst
lmlFunctionParser = (\funname (ret, args) body -> LMLAstFunction funname ret args body) 
    <$> (strP "fun" *> wsP *> (identParser <|> symbolParser) <* wsP)
    <*> (charP '(' *> wsP *> lmlFuncDeclParser <* wsP <* charP ')' <* wsP)
    <*> (charP '=' *> wsP *> lmlParser)


-- example: fun compose f g x :: ('a -> 'b) -> ('a -> 'b) -> 'a -> 'c = (f (g x))
lmlAltFuncParser :: Parser LMLAst
lmlAltFuncParser = 
    ((\name paramNames decltype body -> (name, paramNames, decltype, body))
    <$> (strP "fun" *> wsP *> identParser)
    <*> (wsP *> sepBy wsP identParser)
    <*> (wsP *> strP "::" *> wsP *> sepBy (wsP *> strP "->" <* wsP) lmlTypeParser)
    <*> (wsP *> charP '=' *> wsP *> lmlParser))

    >>= (\(name, paramNames, decltype, body) -> if (length paramNames) == (length (init decltype))
            then succeedP $ LMLAstFunction name (last decltype) (zip paramNames (init decltype)) body
            else failP)


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


lmlCaseExprParser :: Parser LMLAst
lmlCaseExprParser = (\inspected patterns -> LMLAstCaseExpr inspected patterns)
    <$> (strP "case" *> wsP *> lmlParser)
    <*> (wsP *> strP "of" *> wsP *> sepBy wsP patP)
    where
        patP = (,) <$> lmlPatternParser <*> (wsP *> strP "->" *> wsP *> lmlParser)


lmlParser :: Parser LMLAst
lmlParser =
    lmlCaseExprParser <|>
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


lmlPatternParser :: Parser LMLPat
lmlPatternParser = lmlIgnorePattern
    <|> lmlListPattern
    <|> lmlValuePattern
    <|> lmlDataPattern
    where
        lmlIgnorePattern = LMLPatIgnore <$ charP '_'

        lmlValuePattern = LMLPatValue <$> lmlParser

        lmlListPattern 
            =  LMLPatList <$> 
            ((LMLPatListNil <$ (charP '[' <* wsP <* charP ']'))
                <|> (LMLPatListFirstRest 
                    <$> (charP '(' *> wsP *> lmlPatternParser)
                    <*> (wsP *> charP ':' *> wsP *> lmlPatternParser <* wsP <* charP ')'))

                <|> (LMLPatListExplicit <$> (charP '[' *> wsP *> sepBy (wsP *> charP ',' <* wsP) lmlPatternParser <* wsP <* charP ']')))

        lmlDataPattern = Parser $ const Nothing -- TODO
