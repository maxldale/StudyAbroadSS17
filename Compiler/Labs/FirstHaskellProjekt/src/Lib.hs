-- / A Lib module.
module Lib
    ( someFunc
    , square
    ) where

-- / Prints "someFunc"
someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- / Calculate the square of a number
square :: Num a
       => a -- ^ the number
       -> a -- ^ the square
square a = a * a
