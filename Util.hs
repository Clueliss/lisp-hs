module Util where

import Data.Either


unwrapRight :: Either String a -> a
unwrapRight (Left e)  = error e
unwrapRight (Right x) = x


guardWithErr :: String -> Bool -> Either String ()
guardWithErr err True  = Right ()
guardWithErr err False = Left err 

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right x) = Just x

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither x Nothing = Left x
maybeToEither _ (Just y) = Right y
