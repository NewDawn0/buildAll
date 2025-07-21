{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module CLI
  ( CLIOptions (..),
    parseArgs,
    validateOptions,
  )
where

import Build
import Flake (FlakeOutput (..))
import Options.Applicative

data CLIOptions = CLIOptions
  { optAll :: Bool,
    optDevShells :: Bool,
    optFormatters :: Bool,
    optPkgs :: Bool,
    path :: String
  }
  deriving (Show)

cliParser :: Parser CLIOptions
cliParser =
  CLIOptions
    <$> switch
      ( long "all"
          <> short 'a'
          <> help "Build all outputs"
      )
    <*> switch
      ( long "dev-shells"
          <> short 'd'
          <> help "Build all dev shells"
      )
    <*> switch
      ( long "formatters"
          <> short 'f'
          <> help "Build all formatters"
      )
    <*> switch
      ( long "packages"
          <> short 'p'
          <> help "Build all packages"
      )
    <*> argument
      str
      ( metavar "FLAKE_PATH"
          <> help "Path to the flake"
      )

parseArgs :: IO CLIOptions
parseArgs =
  execParser $
    info
      (cliParser <**> helper)
      ( fullDesc
          <> progDesc "Build all outputs of a flake"
          <> header "build-all - Build all outputs of a flake"
      )

validateOptions :: CLIOptions -> FlakeOutput -> IO ()
validateOptions CLIOptions {..} output
  | optAll = buildAllOutputs output
  | optDevShells = buildDevShells output
  | optFormatters = buildFormatters output
  | optPkgs = buildPackages output
  | otherwise = buildAllOutputs output
