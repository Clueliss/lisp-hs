module Util where

import Data.Either


unwrapRight :: Either String a -> a
unwrapRight (Left e)  = error e
unwrapRight (Right x) = x


guardWithErr :: String -> Bool -> Either String ()
guardWithErr err True  = Right ()
guardWithErr err False = Left err 
