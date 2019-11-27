module Parser where

import Control.Applicative
import Data.Char
import Text.Read
import LispValue

newtype Parser a = Parser {
    parse :: String -> Maybe (a, String)
}

instance Functor Parser where
    fmap f p = Parser $ \input -> do
        (val, rest) <- parse p input
        Just (f val, rest)


instance Applicative Parser where
    pure x = Parser $ \input -> Just (x, input)

    (<*>) f p = Parser $ \input -> do
        (func, rest) <- parse f input
        (val, rest'') <- parse p rest

        Just (func val, rest'')


instance Alternative Parser where
    empty = Parser $ \input -> Nothing

    (<|>) p1 p2 = Parser $ \input -> parse p1 input <|> parse p2 input


charP :: Char -> Parser Char
charP x = Parser f
    where
        f (y:ys) | x == y = Just (x, ys)
        f _ = Nothing 


strP :: String -> Parser String
strP = traverse charP


normalCharParser :: Parser Char
normalCharParser = parseIf $ \ch -> ch /= '\\' && ch /= '"'


escapedCharParser :: Parser Char
escapedCharParser = 
    ('"' <$ strP "\"") <|>
    ('\\' <$ strP "\\\\") <|>
    ('\n' <$ strP "\\n") <|>
    ('\t' <$ strP "\\t")


sepBy :: Parser s -> Parser a -> Parser [a]
sepBy sep elem = (:) <$> elem <*> many (sep *> elem) <|> pure []


parseIf :: (Char -> Bool) -> Parser Char
parseIf f = Parser $ g
    where
        g (x:xs) | f x = Just (x, xs)
        g _ = Nothing

        
spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input -> Just (span f input)


wsP :: Parser String
wsP = many (parseIf isSpace)


alphaParser :: Parser Char
alphaParser = parseIf isAlpha


alphaNumParser :: Parser Char
alphaNumParser = parseIf isAlphaNum



lispNumParser :: Parser LispValue
lispNumParser = Parser $ \input -> do
    (num, rest) <- parse (spanP (liftA2 (||) isDigit (== '.'))) input

    if null num
        then Nothing
        else do
            numval <- readMaybe num
            Just (LispNum numval, rest)



lispBoolParser :: Parser LispValue
lispBoolParser = (LispBool True <$ t) <|> (LispBool False <$ f)
    where
        t = strP "true"
        f = strP "false"


lispStrParser :: Parser LispValue
lispStrParser = LispStr <$> (charP '"' *> many (normalCharParser <|> escapedCharParser) <* charP '"')


lispIdentParser :: Parser LispValue
lispIdentParser = LispIdent <$> ((:) <$> alphaParser <*> (many alphaNumParser))

lispListParser :: Parser LispValue
lispListParser = LispList <$> (charP '(' *> (sepBy wsP lispValueParser) <* charP ')')


lispValueParser :: Parser LispValue
lispValueParser =
    lispBoolParser <|>
    lispStrParser <|>
    lispNumParser <|>
    lispIdentParser <|>
    lispListParser
