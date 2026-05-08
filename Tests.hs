module Main where

import Parser
import TypeCheck
import Eval
import Syntax
import Data.List (isInfixOf, isPrefixOf)

data Expected
  = ExpectValue Value
  | ExpectTypeError String
  | ExpectParseError
  deriving (Eq, Show)

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

matchesExpected :: Expected -> Either String Value -> Bool
matchesExpected (ExpectValue v) (Right actual) = v == actual
matchesExpected (ExpectTypeError msg) (Left actual) = msg `isInfixOf` actual
matchesExpected ExpectParseError (Left actual) = "Parse error:" `isPrefixOf` actual
matchesExpected _ _ = False

testCases :: [(String, Expected)]
testCases =
  [ ("1 + 2", ExpectValue (VInt 3))
  , ("if true then 10 else 20", ExpectValue (VInt 10))
  , ("let x = 5 in x + 2", ExpectValue (VInt 7))
  , ("(\\x : Int -> x + 1) 5", ExpectValue (VInt 6))
  , ("Cons 5 (Nil[Int])", ExpectValue (VCons (VInt 5) VNil))
  , ("case Cons 5 (Nil[Int]) of Nil -> 0 | Cons x xs -> x", ExpectValue (VInt 5))
  , ("1 + true", ExpectTypeError "Type error:")
  , ("if 1 then 2 else 3", ExpectTypeError "Type error:")
  , ("if true 1 else 0", ExpectParseError)
  ]

runTest :: Int -> (String, Expected) -> IO Bool
runTest n (input, expected) = do
  let result = runProgram input
  let ok = matchesExpected expected result
  putStrLn ("Test " ++ show n ++ ": " ++ if ok then "PASS" else "FAIL")
  putStrLn ("  Input:    " ++ input)
  putStrLn ("  Expected: " ++ show expected)
  putStrLn ("  Actual:   " ++ show result)
  putStrLn ""
  return ok

runAllTests :: IO ()
runAllTests = do
  results <- sequence [runTest n tc | (n, tc) <- zip [1..] testCases]
  let passed = length (filter id results)
  let total = length results
  putStrLn ("Summary: " ++ show passed ++ "/" ++ show total ++ " tests passed.")

main :: IO ()
main = runAllTests