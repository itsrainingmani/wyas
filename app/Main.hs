module Main where

import System.Environment

-- IO () is an IO Action carrying along values of unit type ()
main :: IO ()
main = do
  args <- getArgs
  -- read 2 nums, add them and print the result
  print (read (head args) + read (args !! 1))
