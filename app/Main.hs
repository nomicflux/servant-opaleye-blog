module Main where

import System.Environment
import Lib

main :: IO ()
main = getArgs >>= startApp
