module Main where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import System.Environment (getArgs)
import Text.Megaparsec.Error

import Ast
import Parser
import Tycheck


main :: IO ()
main = do
  args <- getArgs

  let (filename, api_filename) =
        case args of
          (f:a:_) -> (f, Just a)
          f:_   -> (f, Nothing)
          []    -> error "Error: no input file"

  -- Read in source file
  src <- readFile filename

  -- putStrLn $ case parse filename src of
  --              Left err -> errorBundlePretty err
  --              Right cls -> show cls

  let cls = case parse filename src of
              Left err -> error $ errorBundlePretty err
              Right cls -> cls

  putStrLn $ show cls

  api_text <- mapM BS.readFile api_filename

  let api_val = (join $ decode <$> api_text) :: Maybe Value
  -- case api_text of
  --   Just s -> putStrLn $ show (decode s :: Maybe Value)
  --   Nothing -> return ()

  -- putStrLn $ show api_text
  -- putStrLn $ show $ f <$> val
  -- case fromJust val of
  --   Array a -> putStrLn $ show $ V.length a

  let tychecked = tycheckMain cls api_val
  
  putStrLn $ show tychecked

  putStrLn $ "hecc"
  case api_val of
    Just _ -> return ()
    Nothing -> putStrLn "WARNING: no api file provided."


f :: Value -> String
f (Object _) = "Object"
f (Array _) = "Array"
f (String _) = "String"
f (Number _) = "Number"
f (Bool _) = "Bool"
f Null = "Null"
