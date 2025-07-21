{-# LANGUAGE NamedFieldPuns #-}

module Build
  ( buildAllOutputs,
    buildDevShells,
    buildFormatters,
    buildPackages,
  )
where

import Colour (Colour (..), colourify, fixLine, tmpLine)
import Flake (FlakeOutput (..))
import System.Process (callProcess)

mkFlakeAttr :: String -> String -> String -> String
mkFlakeAttr prefix systemArch pkg = prefix ++ systemArch ++ "." ++ pkg

build :: FlakeOutput -> String -> IO ()
build FlakeOutput {flakePath} pkgAttr = do
  tmpLine $ colourify Yellow "> " ++ "Building " ++ pkgAttr
  out <- callProcess "nix" ["build", flakePath ++ "#" ++ pkgAttr]
  fixLine $ colourify Green ">" ++ " Built " ++ pkgAttr
  return out

buildPackages :: FlakeOutput -> IO ()
buildPackages out@FlakeOutput {packages} = do
  mapM_ (build out) packages

buildDevShells :: FlakeOutput -> IO ()
buildDevShells out@FlakeOutput {devShells, systemArch} = do
  mapM_ (build out . mkFlakeAttr "devShells." systemArch) devShells

buildFormatters :: FlakeOutput -> IO ()
buildFormatters out@FlakeOutput {formatters, systemArch} = do
  mapM_ (build out . mkFlakeAttr "formatter." systemArch) formatters

buildAllOutputs :: FlakeOutput -> IO ()
buildAllOutputs out = do
  buildPackages out
  buildDevShells out
  buildFormatters out
