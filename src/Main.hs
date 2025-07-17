module Main where

import CLI (CLIOptions (..), parseArgs, validateOptions)
import Flake (getFlakeOutput)

main :: IO ()
main = do
  opts <- parseArgs
  result <- getFlakeOutput $ path opts
  case result of
    Just output -> do
      validateOptions opts output
      putStrLn "> Done"
    Nothing -> putStrLn "No flake output found"
