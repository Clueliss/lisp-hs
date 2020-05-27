module Parser where

import Control.Applicative
import Control.Monad (guard)
import Data.Char
import Text.Read


newtype Parser a = Parser {
    runParser :: String -> Maybe (a, String)
}


instance Functor Parser where
    fmap f p = Parser $ \input -> do
        (val, rest) <- runParser p input
        Just (f val, rest)


instance Applicative Parser where
    pure x = Parser $ \input -> Just (x, input)

    (<*>) f p = Parser $ \input -> do
        (func, rest) <- runParser f input
        (val, rest'') <- runParser p rest

        Just (func val, rest'')


instance Alternative Parser where
    empty = Parser $ \input -> Nothing

    (<|>) p1 p2 = Parser $ \input -> runParser p1 input <|> runParser p2 input


instance Monad Parser where
    (>>=) (Parser p) f = Parser $ \input -> case p input of
        Just (x, rest) -> runParser (f x) rest
        Nothing        -> Nothing


succeedP :: a -> Parser a
succeedP = pure

failP :: Parser a
failP = Parser $ const Nothing


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


charParser :: Parser Char
charParser = normalCharParser <|> escapedCharParser


sepBy :: Parser s -> Parser a -> Parser [a]
sepBy sep elem = (:) <$> elem <*> many (sep *> elem) <|> pure []


parseIf :: (Char -> Bool) -> Parser Char
parseIf f = Parser $ g
    where
        g (x:xs) | f x = Just (x, xs)
        g _ = Nothing


parseOpt :: Parser a -> a -> Parser a
parseOpt p def = Parser $ f
    where
        f input = case runParser p input of
            Just x   -> Just x
            Nothing  -> Just (def, input)
        

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input -> Just (span f input)


nonEmpty :: Parser [a] -> Parser [a]
nonEmpty p = Parser $ \input -> do
    (val, rest) <- runParser p input

    guard (not (null val))

    Just (val, rest)


wsP :: Parser String
wsP = many (parseIf isSpace)


alphaParser :: Parser Char
alphaParser = parseIf isAlpha


alphaNumParser :: Parser Char
alphaNumParser = parseIf isAlphaNum


identParser :: Parser String
identParser = (:) <$> (alphaParser <|> charP '_') <*> (many (alphaNumParser <|> charP '_'))


floatParser :: Parser Double
floatParser = Parser $ \input -> do
    (num, rest) <- runParser (spanP (liftA2 (||) isDigit (== '.'))) input
    
    guard (not (null num))

    numval <- readMaybe num
    Just (numval, rest)


maybeParse :: Parser a -> Parser (Maybe a)
maybeParse p = Parser $ \input -> Just (f (runParser p input) input)
    where
        f :: Maybe (a, String) -> String -> (Maybe a, String)
        f (Just (val, rest)) _ = (Just val, rest)
        f Nothing input' = (Nothing, input')
