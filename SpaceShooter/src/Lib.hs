module Lib
    ( someFunc,
    (+/+)
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"


(+/+) :: Int -> Int -> Int -> Int
(+/+) a b c = a + b + c