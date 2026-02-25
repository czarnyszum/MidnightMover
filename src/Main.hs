
module Main where

import Ctx

main :: IO ()
main =
  do
    maybeUser <- readUser "."
    print maybeUser
  
