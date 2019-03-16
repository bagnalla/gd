module Main where

import System.Environment (getArgs)

import Text.Megaparsec.Error

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

  putStrLn $ case parse filename src of
               Left err -> errorBundlePretty err
               Right cls -> show cls

  putStrLn $ "hecc"
