module Main where

import Flake (getFlakeOutput)
import CLI (CLIOptions(..), cliParser, validateOptions)
import Options.Applicative

main :: IO ()
main = do
  opts <- execParser $ info (cliParser <**> helper)
    ( fullDesc
      <> progDesc "Build all outputs of a flake"
      <> header "build-all - Build all outputs of a flake" )
  result <- getFlakeOutput (path opts)
  case result of
    Just output -> do
      validateOptions opts output
      putStrLn "Done"
    Nothing -> putStrLn "No flake output found"
