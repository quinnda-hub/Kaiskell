module Main where

import qualified Game (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Game.someFunc
