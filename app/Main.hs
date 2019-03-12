module Main where

import System.Environment (getArgs)

import Ast
import Parser


main :: IO ()
main = do
  args <- getArgs

  let filename = case args of
                   f:_ -> f
                   []  -> error "Error: no input file"

  -- Read in source file
  src <- readFile filename

  let cls = parse filename src

  putStrLn $ show cls

  putStrLn $ "hecc"
