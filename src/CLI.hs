{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module CLI
  ( CLIOptions (..),
    cliParser,
    validateOptions,
  )
where

import Build (buildAllOutputs, buildDevShells, buildPackages)
import Flake (FlakeOutput (..))
import Options.Applicative

data CLIOptions = CLIOptions
  { optAll :: Bool,
    optPkgs :: Bool,
    optDevShells :: Bool,
    path :: String
  }
  deriving (Show)

cliParser :: Parser CLIOptions
cliParser =
  CLIOptions
    <$> switch
      ( long "packages"
          <> short 'p'
          <> help "Build all packages"
      )
    <*> switch
      ( long "dev-shells"
          <> short 'd'
          <> help "Build all dev shells"
      )
    <*> switch
      ( long "all"
          <> short 'a'
          <> help "Build all outputs"
      )
    <*> argument
      str
      ( metavar "FLAKE_PATH"
          <> help "Path to the flake"
      )

validateOptions :: CLIOptions -> FlakeOutput -> IO ()
validateOptions CLIOptions {..} output
  | optAll = buildAllOutputs output
  | optPkgs = buildPackages output
  | optDevShells = buildDevShells output
  | otherwise = putStrLn "No outputs to build"
