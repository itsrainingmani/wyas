module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

-- Define a parser that recognizes one of the symbols allowed in Scheme identifiers
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
  Left err -> "No match:" ++ show err
  Right _val -> "Found value"

-- IO () is an IO Action carrying along values of unit type ()
main :: IO ()
main = do
  (expr : _) <- getArgs
  putStrLn (readExpr expr)

-- putStrLn "Please enter your name:\n"
-- name <- getLine
-- putStrLn ("Hello, " ++ name)

-- args <- getArgs
-- read 2 nums, add them and print the result
-- print (read (head args) + read (args !! 1))
