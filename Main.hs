module Main where

import Syntax
import Parser
import TypeCheck
import Eval
import Text.ParserCombinators.Parsec (ParseError)

runProgram :: String -> Either String Value
runProgram input =
  case parseString input of
    Left err ->
      Left ("Parse error: " ++ show err)

    Right expr ->
      case typeCheck [] expr of
        Left typeErr ->
          Left ("Type error: " ++ typeErr)

        Right _ ->
          eval [] expr

main :: IO ()
main = do
  putStrLn "Enter a program, then press Ctrl-D when finished:"
  input <- getContents
  case runProgram input of
    Left err ->
      putStrLn err
    Right value ->
      print value