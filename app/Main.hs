module Main where

import System.Environment
import MyLib

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)
